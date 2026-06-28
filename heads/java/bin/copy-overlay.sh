#!/usr/bin/env bash
# Overlay hand-written Java source onto a dist package, so the published Maven
# artifact for that package is self-contained.
#
# Generalizes the kernel-runtime overlay (#418) to any package (#511): a
# distribution package is generated modules PLUS copied overlay files, per the
# governing equation
#   dist/java/<pkg>/ = transform(packages/<pkg>/) + copy(overlay/java/<pkg>/)
# (see docs/build-system.md). The overlay tree's canonical home is the top-level
# overlay/java/<pkg>/src/main/java/ — a sibling of dist/, packages/, heads/,
# bindings/. This is a dumb full-tree merge: every file under the overlay tree is
# copied, leaving generated siblings under the dist package untouched (#418).
#
# Only the copy step (this script) reads overlay/ — the overlay-only-copy
# invariant. Nothing here is package-specific: hydra-kernel is simply the package
# whose overlay tree always exists (the kernel runtime); other packages
# (hydra-pg, hydra-rdf, ...) carry an overlay tree only when they have
# hand-written host-native source to inject.
#
# If the overlay tree for <pkg> does not exist, this is a no-op (a package with
# no hand-written Java needs no overlay).
#
# Usage:
#   copy-overlay.sh <pkg> [--dist-root <dir>] [--manifest <file>]
#
# --manifest <file> appends '<OUT_DIR>\t<relPath>' lines (tab-separated) for
# every file under OUT_DIR. Consumed by bootstrap-from-json --keep-paths-from to
# protect hand-copied overlay files from --prune-stale (#357).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>] [--manifest <file>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/java"
MANIFEST_FILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --manifest)  MANIFEST_FILE="$2"; shift 2 ;;
        *) shift ;;
    esac
done

# Copy the WHOLE overlay src/ tree (not just main/java): a binding overlay may
# carry main/java/ (hand-written classes), main/antlr/ (ANTLR .g4 grammars),
# main/resources/, and test/ — all must land in the dist so the package builds
# (#511). Operating at the src/ level preserves that substructure and matches the
# governing equation dist = transform(packages) + copy(overlay) verbatim.
OVERLAY_SRC="$HYDRA_ROOT_DIR/overlay/java/$PACKAGE/src"
OUT_SRC="$DIST_ROOT/$PACKAGE/src"

if [ ! -d "$OVERLAY_SRC" ]; then
    # No overlay tree for this package: nothing to copy. Not an error — most
    # packages have no hand-written source.
    exit 0
fi

mkdir -p "$OUT_SRC"

# Merge the entire overlay src/ tree onto the generated dist. Trailing /. on the
# source copies CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_SRC/." "$OUT_SRC/"

if [ -n "$MANIFEST_FILE" ]; then
    # Record EVERY overlay file into the keep-paths manifest so neither prune
    # path deletes hand-written overlay source:
    #   - bootstrap-from-json --prune-stale (full regen, #357)
    #   - digest-check fresh --keep-paths-from (#393 cache-skip reconcile, #511)
    # BOTH key the keep-set by SOURCE-SET dir (e.g. .../src/main/java) and match
    # paths relative to THAT dir — not the package src/ root. The overlay layout
    # is src/<config>/<lang>/<rel> (e.g. main/java/hydra/..., test/java/...,
    # main/antlr/...), so the source-set dir is the first two components. Emit
    # '<OUT_SRC>/<config>/<lang>\t<rel>' per file. Walk the overlay tree itself
    # (authoritative source list) so only overlay files are protected.
    ( cd "$OVERLAY_SRC" && find . -type f -print | sed 's|^\./||' \
        | awk -v base="$OUT_SRC" -F/ '{
              ss = $1 "/" $2;                         # e.g. main/java
              rel = substr($0, length(ss) + 2);       # path after "main/java/"
              printf "%s/%s\t%s\n", base, ss, rel;
          }' \
        >> "$MANIFEST_FILE" )
fi

echo "  Overlaid hand-written overlay source from overlay/java/$PACKAGE/ into $OUT_SRC/"
