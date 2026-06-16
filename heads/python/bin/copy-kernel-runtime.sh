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

# The test bridge (hydra/test/test_env.py) is the runtime counterpart of the
# generated hydra.test.test_graph module, which imports hydra.test.test_env and
# must resolve under the dist tree at test time. Per #434 it lives in the overlay
# test tree (overlay/python/hydra-kernel/src/test/python/), so this one copy step
# is the only thing that reads overlay/ — the old heads/ Step 0a special-case in
# assemble-distribution.sh / assemble-all.sh is gone.
OVERLAY_TEST_DIR="$HYDRA_ROOT_DIR/overlay/python/hydra-kernel/src/test/python"
OUT_TEST_DIR="$DIST_ROOT/hydra-kernel/src/test/python"

if [ ! -d "$OVERLAY_DIR" ]; then
    echo "error: missing overlay dir $OVERLAY_DIR" >&2
    exit 1
fi

# append_manifest <dir>: record every file under <dir> in the keep-paths manifest
# so bootstrap-from-json --prune-stale won't remove the hand-copied files (#357).
append_manifest() {
    local dir="$1"
    [ -n "$MANIFEST_FILE" ] || return 0
    ( cd "$dir" && find . -type f -print | sed 's|^\./||' \
        | awk -v d="$dir" '{ printf "%s\t%s\n", d, $0 }' \
        >> "$MANIFEST_FILE" )
}

mkdir -p "$OUT_DIR/hydra"

# Merge the entire overlay main tree onto the generated kernel dist. Trailing /.
# copies CONTENTS into the dest, leaving generated siblings untouched.
cp -R "$OVERLAY_DIR/." "$OUT_DIR/"

# Don't carry __pycache__ directories into the published artifact.
find "$OUT_DIR" -type d -name __pycache__ -prune -exec rm -rf {} +

append_manifest "$OUT_DIR"

echo "  Overlaid hand-written Python kernel runtime from overlay/python/ into $OUT_DIR/hydra/"

# Merge the overlay test tree (the test bridge) onto the generated kernel test dist.
if [ -d "$OVERLAY_TEST_DIR" ]; then
    mkdir -p "$OUT_TEST_DIR"
    cp -R "$OVERLAY_TEST_DIR/." "$OUT_TEST_DIR/"
    find "$OUT_TEST_DIR" -type d -name __pycache__ -prune -exec rm -rf {} +
    append_manifest "$OUT_TEST_DIR"
    echo "  Overlaid hand-written Python kernel test runtime from overlay/python/ into $OUT_TEST_DIR/hydra/"
fi
