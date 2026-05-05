#!/usr/bin/env bash
# Copy the hand-written Python runtime support from heads/python/src/main/python/
# into dist/python/hydra-kernel/src/main/python/ so the published hydra-kernel
# Python wheel is self-contained.
#
# Per the 0.15 layout, hydra-kernel is special: it ships with not only the
# generated kernel modules but also the runtime classes (hydra/{lib,dsl,sources}/
# plus tools.py, py.typed, __init__.py) that every Hydra Python program needs.
#
# bootstrap.py and generation.py are intentionally NOT copied: they import every
# coder (hydra.java.coder, hydra.python.coder, hydra.scala.coder, hydra.lisp.coder)
# and so live above the kernel layer. They remain in heads/python for the
# developer rollup and demos.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_PYTHON_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/python"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

SRC_DIR="$HYDRA_PYTHON_HEAD/src/main/python"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/python"

if [ ! -d "$SRC_DIR" ]; then
    echo "error: missing source dir $SRC_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR/hydra"

# Top-level kernel marker / utility files. We deliberately do NOT copy
# hydra/__init__.py: each per-package wheel contributes to the `hydra`
# namespace via PEP 420 implicit namespace packages (no __init__.py at the
# namespace root), so multiple wheels can install side-by-side and have
# their hydra.* contents merge at import time. Including any one wheel's
# __init__.py would convert hydra/ into a regular package and shadow the
# others.
for f in py.typed tools.py; do
    if [ -f "$SRC_DIR/hydra/$f" ]; then
        cp "$SRC_DIR/hydra/$f" "$OUT_DIR/hydra/$f"
    fi
done

# Kernel subpackages: lib (primitive impls), dsl (Python-side DSL helpers),
# sources (libraries.py registers primitive names). MERGE into the existing
# tree (which already contains generated kernel modules under
# hydra/core/, hydra/json/, etc.) — using rm -rf <sub> + cp -R would clobber
# generated children. Trailing /. on the source is portable across BSD and GNU cp.
for sub in lib dsl sources; do
    if [ -d "$SRC_DIR/hydra/$sub" ]; then
        mkdir -p "$OUT_DIR/hydra/$sub"
        cp -R "$SRC_DIR/hydra/$sub/." "$OUT_DIR/hydra/$sub/"
    fi
done

# Don't carry __pycache__ directories into the published artifact.
find "$OUT_DIR" -type d -name __pycache__ -prune -exec rm -rf {} +

echo "  Copied hand-written Python runtime into $OUT_DIR/hydra/"
