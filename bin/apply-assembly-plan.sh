#!/usr/bin/env bash
# Native executor for the promoted assembly plan (#416, T2/T4 dedup).
#
# The DECISION — which overlay source trees map to which dist trees, and which
# files a prune pass must protect — is promoted, translingual data:
# hydra.build.assemblyplan.deriveAssemblyPlan. That plan is PURE PATH-CONCATENATION:
#   overlay/<lang>/<pkg>/<subdir>  ->  dist/<lang>/<pkg>/<subdir>   (kind: merge)
#   keep-path: (dist/<lang>/<pkg>/<sourceSetDir>, relPath) per overlay file
# There is nothing to "evaluate at runtime" — so this executor computes the same
# mapping inline as a DATA-DRIVEN FOLD over the overlay tree's find-results (string
# concat parameterised by <lang>/<pkg>, NOT a case on package/lang names — a name
# branch would be the only neutrality violation). Zero execute-time host dependency:
# no stack, no java, no jq — the actual host-independence goal.
#
# The DSL 'deriveAssemblyPlan' stays the AUTHORITATIVE ORACLE: bin/test-assembly-plan-
# conformance.sh asserts this script's derived plan byte-matches the DSL's output, so
# the bash fold can never silently drift from the promoted spec. The DSL is checked at
# TEST time, never invoked at run time.
#
# Replaces the per-host copy-overlay.sh inline logic (java 95L, python 85L, ...) with
# one host-independent executor. Byte-identical to copy-overlay.sh by construction:
# same whole-src cp -R merge, same '<sourceSetDir>\t<relPath>' keep-manifest contract
# that bootstrap-from-json --prune-stale (#357) and digest-check fresh --keep-paths-from
# (#393/#511) both key on.
#
# Usage:
#   apply-assembly-plan.sh <lang> <pkg> [--dist-root <dir>] [--manifest <file>]
#
# --manifest <file> appends the keep-paths so the prune passes protect the hand-copied
# overlay files. No overlay tree for <pkg> => no-op.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <lang> <package> [--dist-root <dir>] [--manifest <file>]" >&2
    exit 1
fi

LANG_NAME="$1"
PACKAGE="$2"
shift 2

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/$LANG_NAME"
MANIFEST_FILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --manifest)  MANIFEST_FILE="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OVERLAY_SRC="$HYDRA_ROOT_DIR/overlay/$LANG_NAME/$PACKAGE/src"
OUT_SRC="$DIST_ROOT/$PACKAGE/src"

# No overlay tree for this package: nothing to assemble. Not an error — most
# packages carry no hand-written overlay source.
if [ ! -d "$OVERLAY_SRC" ]; then
    exit 0
fi

# --- The plan, derived inline (pure path-concat, data-driven over find) ---
#
# overlayEntries: one merge entry per source-set subdir that exists under the overlay
# src/ tree. The whole src/ tree is one merge (cp -R src/. -> dist .../src/), exactly
# as copy-overlay.sh does; the plan models it as overlay/<lang>/<pkg>/src ->
# dist/<lang>/<pkg>/src, kind merge. (availableTrees is the src-level tree; the finer
# per-source-set granularity below is only needed for the keep-paths contract.)
mkdir -p "$OUT_SRC"
cp -R "$OVERLAY_SRC/." "$OUT_SRC/"

# keepPathsFor: one (sourceSetDir, relPath) per overlay file. The prune consumers key
# by SOURCE-SET dir = dist/<lang>/<pkg>/src/<config>/<lang> (first two path components
# under src/, e.g. main/java), with relPath relative to THAT dir. Fold over the overlay
# file list, remapping each overlay path to its dist source-set dir. This is the pure
# 'keepPathsFor' composed with 'remapDest' — a string map, no name branch.
if [ -n "$MANIFEST_FILE" ]; then
    ( cd "$OVERLAY_SRC" && find . -type f -print | sed 's|^\./||' \
        | awk -v out_src="$OUT_SRC" -F/ 'NF>=3 {
              ss = $1 "/" $2;                        # <config>/<lang>, e.g. main/java
              rel = substr($0, length(ss) + 2);      # path after "<config>/<lang>/"
              printf "%s/%s\t%s\n", out_src, ss, rel;
          }' \
        >> "$MANIFEST_FILE" )
fi

echo "  Applied assembly plan: overlaid overlay/$LANG_NAME/$PACKAGE/ into $OUT_SRC/"
