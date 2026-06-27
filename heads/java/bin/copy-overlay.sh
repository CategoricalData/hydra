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

OVERLAY_DIR="$HYDRA_ROOT_DIR/overlay/java/$PACKAGE/src/main/java"
OUT_DIR="$DIST_ROOT/$PACKAGE/src/main/java"

if [ ! -d "$OVERLAY_DIR" ]; then
    # No overlay tree for this package: nothing to copy. Not an error — most
    # packages have no hand-written Java.
    exit 0
fi

mkdir -p "$OUT_DIR"

# Merge the entire overlay tree onto the generated dist. Trailing /. on the
# source copies CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_DIR/." "$OUT_DIR/"

if [ -n "$MANIFEST_FILE" ]; then
    # Emit '<OUT_DIR>\t<relPath>' for every regular file currently under OUT_DIR.
    ( cd "$OUT_DIR" && find . -type f -print | sed 's|^\./||' \
        | awk -v dir="$OUT_DIR" '{ printf "%s\t%s\n", dir, $0 }' \
        >> "$MANIFEST_FILE" )
fi

echo "  Overlaid hand-written Java source from overlay/java/$PACKAGE/ into $OUT_DIR/"
