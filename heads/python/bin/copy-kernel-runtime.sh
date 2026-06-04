#!/usr/bin/env bash
# Overlay the hand-written Python kernel runtime onto dist/python/hydra-kernel/ so
# the published hydra-kernel Python wheel is self-contained.
#
# The runtime's canonical home is the top-level overlay tree
# overlay/python/hydra-kernel/src/main/python/ (a sibling of dist/, packages/,
# heads/, bindings/ — see docs/build-system.md). It holds exactly the hand-written
# modules the kernel needs at runtime (hydra/{lib,dsl,sources}/ plus tools.py and
# the py.typed markers) — nothing else. Because the overlay tree contains ONLY
# runtime, this is a dumb full-tree merge: no selective file lists (#418). Compare
# the Java analog (heads/java/bin/copy-kernel-runtime.sh) and the Haskell analog
# (sync-haskell.sh's overlay step).
#
# Files that are NOT kernel runtime stay in heads/python/src/main/python/ and are
# NOT copied here:
#   - bootstrap.py, generation.py — import every coder; live above the kernel layer
#   - hydra/python/ — the Python coder itself (its own package, hydra-python)
#   - hydra/__init__.py — deliberately NOT part of the overlay: each per-package
#     wheel contributes to the `hydra` namespace via PEP 420 implicit namespace
#     packages (no __init__.py at the namespace root), so wheels install
#     side-by-side and merge at import time. (It is therefore absent from the
#     overlay tree, so a full-tree copy cannot reintroduce it.)
#
# The merge (cp -R contents) leaves generated siblings under hydra/<sub>/ untouched.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>] [--manifest <file>]
#
# --manifest <file> appends '<OUT_DIR>\t<relPath>' lines (tab-separated) for
# every file under OUT_DIR (see heads/java/bin/copy-kernel-runtime.sh).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_PYTHON_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/python"
MANIFEST_FILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --manifest)  MANIFEST_FILE="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OVERLAY_DIR="$HYDRA_ROOT_DIR/overlay/python/hydra-kernel/src/main/python"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/python"

if [ ! -d "$OVERLAY_DIR" ]; then
    echo "error: missing overlay dir $OVERLAY_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR/hydra"

# Merge the entire overlay tree onto the generated kernel dist. Trailing /. copies
# CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_DIR/." "$OUT_DIR/"

# Don't carry __pycache__ directories into the published artifact.
find "$OUT_DIR" -type d -name __pycache__ -prune -exec rm -rf {} +

if [ -n "$MANIFEST_FILE" ]; then
    ( cd "$OUT_DIR" && find . -type f -print | sed 's|^\./||' \
        | awk -v dir="$OUT_DIR" '{ printf "%s\t%s\n", dir, $0 }' \
        >> "$MANIFEST_FILE" )
fi

echo "  Overlaid hand-written Python kernel runtime from overlay/python/ into $OUT_DIR/hydra/"
