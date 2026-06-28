#!/usr/bin/env bash
# Overlay hand-written Python source for one distribution package onto its
# dist/python/<pkg>/ tree, so the published wheel is self-contained.
#
# Generalized from the kernel-only copy-kernel-runtime.sh (#418) to ANY package
# (#511): the Python analog of heads/java/bin/copy-overlay.sh. A package's
# hand-written source lives in overlay/python/<pkg>/src/ (a sibling of dist/,
# packages/, heads/, bindings/ — see docs/build-system.md); this is the ONLY
# reader of overlay/. No-op for packages with no overlay/python/<pkg>/ tree.
#
# The whole src/ tree is copied (main/python/, test/python/, ...) so the
# substructure is preserved, matching the governing equation
#   dist = transform(packages) + copy(overlay)
# The merge (cp -R contents) leaves generated siblings untouched.
#
# Usage:
#   copy-overlay.sh <package> [--dist-root <dir>] [--manifest <file>]
#
# --manifest <file> appends keep-paths lines so neither prune path deletes the
# hand-written overlay source:
#   - bootstrap-from-json --prune-stale (full regen, #357)
#   - digest-check fresh --keep-paths-from (#393 cache-skip reconcile, #511)
# BOTH key the keep-set by SOURCE-SET dir (e.g. .../src/main/python) and match
# paths relative to THAT dir, so each line is '<OUT_SRC>/<config>/python\t<rel>'.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_PYTHON_HEAD/../.." && pwd )"

PACKAGE=""
DIST_ROOT="$HYDRA_ROOT_DIR/dist/python"
MANIFEST_FILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --manifest)  MANIFEST_FILE="$2"; shift 2 ;;
        --*)         shift ;;
        *)           PACKAGE="$1"; shift ;;
    esac
done

if [ -z "$PACKAGE" ]; then
    echo "usage: copy-overlay.sh <package> [--dist-root <dir>] [--manifest <file>]" >&2
    exit 2
fi

OVERLAY_SRC="$HYDRA_ROOT_DIR/overlay/python/$PACKAGE/src"
OUT_SRC="$DIST_ROOT/$PACKAGE/src"

if [ ! -d "$OVERLAY_SRC" ]; then
    # No overlay tree for this package: nothing to copy. Not an error — most
    # packages have no hand-written Python.
    exit 0
fi

mkdir -p "$OUT_SRC"

# Merge the entire overlay src/ tree onto the generated dist. Trailing /. on the
# source copies CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_SRC/." "$OUT_SRC/"

# Don't carry __pycache__ directories into the published artifact.
find "$OUT_SRC" -type d -name __pycache__ -prune -exec rm -rf {} +

if [ -n "$MANIFEST_FILE" ]; then
    # Record EVERY overlay file keyed by SOURCE-SET dir (e.g. .../src/main/python),
    # relative to that dir, matching what both prune paths key on. The overlay
    # layout is src/<config>/python/<rel> (e.g. main/python/hydra/...,
    # test/python/hydra/...), so the source-set dir is the first two components.
    # Walk the overlay tree itself (authoritative source list) so only overlay
    # files — not generated siblings — are protected. Skip __pycache__.
    ( cd "$OVERLAY_SRC" && find . -type f -not -path '*/__pycache__/*' -print \
        | sed 's|^\./||' \
        | awk -v base="$OUT_SRC" -F/ '{
              ss = $1 "/" $2;                         # e.g. main/python
              rel = substr($0, length(ss) + 2);       # path after "main/python/"
              printf "%s/%s\t%s\n", base, ss, rel;
          }' \
        >> "$MANIFEST_FILE" )
fi

echo "  Overlaid hand-written Python overlay source from overlay/python/$PACKAGE/ into $OUT_SRC/"
