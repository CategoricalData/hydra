#!/usr/bin/env bash
# Copy the hand-written TypeScript runtime support from
# heads/typescript/src/main/typescript/ into
# dist/typescript/hydra-kernel/src/main/typescript/ so that the published
# kernel can be consumed standalone.
#
# Per the 0.15 layout, hydra-kernel is special: it ships not only the
# generated kernel `.ts` files but also the runtime (`hydra/core.ts`,
# `hydra/lib/*.ts`, `hydra/primitives.ts`) that every Hydra TypeScript
# program needs.
#
# Usage:
#   copy-kernel-runtime.sh [--dist-root <dir>]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/typescript"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

SRC_DIR="$HYDRA_TS_HEAD/src/main/typescript"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/typescript"

if [ ! -d "$SRC_DIR" ]; then
    echo "error: missing source dir $SRC_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR/hydra"

# Top-level runtime files (core types, primitive registry).
for f in core.ts primitives.ts; do
    if [ -f "$SRC_DIR/hydra/$f" ]; then
        cp "$SRC_DIR/hydra/$f" "$OUT_DIR/hydra/$f"
    fi
done

# Subpackages: lib (primitive impls). Merge into existing tree (which
# already contains generated kernel modules), preserving generated children.
for sub in lib; do
    if [ -d "$SRC_DIR/hydra/$sub" ]; then
        mkdir -p "$OUT_DIR/hydra/$sub"
        cp -R "$SRC_DIR/hydra/$sub/." "$OUT_DIR/hydra/$sub/"
    fi
done

echo "  Copied hand-written TypeScript runtime into $OUT_DIR/hydra/"

# Test-tree hand-written modules (testEnv.ts) — copied if the test source
# dir exists. testEnv mirrors the role of the Python/Java/Scala/Lisp
# hand-written equivalents: the DSL declares the FQNs so the coder can
# resolve references during inference, but the actual runtime values are
# provided per-language at test time.
TEST_SRC_DIR="$HYDRA_TS_HEAD/src/test/typescript"
TEST_OUT_DIR="$DIST_ROOT/hydra-kernel/src/test/typescript"
if [ -d "$TEST_SRC_DIR/hydra/test" ]; then
    mkdir -p "$TEST_OUT_DIR/hydra/test"
    for f in "$TEST_SRC_DIR"/hydra/test/*.ts; do
        [ -f "$f" ] || continue
        cp "$f" "$TEST_OUT_DIR/hydra/test/"
    done
    echo "  Copied hand-written TypeScript test runtime into $TEST_OUT_DIR/hydra/test/"
fi
