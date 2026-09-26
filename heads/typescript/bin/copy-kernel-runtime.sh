#!/usr/bin/env bash
# Copy the hand-written TypeScript runtime support from the top-level overlay
# tree overlay/typescript/hydra-kernel/src/main/typescript/ into
# dist/typescript/hydra-kernel/src/main/typescript/ so that the published
# kernel can be consumed standalone. This is the TypeScript analog of the
# Java/Python copy-kernel-runtime.sh and the Haskell overlay-kernel-runtime.sh
# (#434): the canonical hand-written sources live, uncompiled, under the
# top-level overlay/ tree (a sibling of dist/, packages/, heads/), and this
# script is the ONLY thing that reads overlay/. The head itself depends only
# on the dist/ copy.
#
# Per the 0.15 layout, hydra-kernel is special: it ships not only the
# generated kernel `.ts` files but also the runtime (`hydra/core/runtime.ts`,
# `hydra/core/overlay/typescript/lib/*.ts`, `hydra/core/primitives.ts`) that
# every Hydra TypeScript program needs.
#
# This is a dumb full-tree merge (cp -R), matching the Python analog
# (heads/python/bin/copy-kernel-runtime.sh) rather than a hardcoded list of
# top-level file/subdir names: a fixed allowlist silently stops copying
# anything the moment the overlay's internal layout shifts. It DID shift, in
# the #729 module-grammar rename (6c09ba4761), which moved every overlay
# source file from hydra/{runtime.ts,lib/,overlay/typescript/lib/,...} to
# hydra/core/{runtime.ts,overlay/typescript/lib/,...} — the old hardcoded
# `for f in runtime.ts primitives.ts bootstrap.ts` / `for sub in lib overlay`
# loops kept exiting 0 while silently copying nothing, leaving dist/typescript
# missing the whole runtime (#729 follow-up). A full-tree copy is immune to
# this class of drift: preserving generated siblings under the dist package
# untouched only requires the overlay tree to contain solely hand-written
# runtime, which is already true for every other host's overlay.
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

OVERLAY_ROOT="$HYDRA_ROOT_DIR/overlay/typescript/hydra-kernel/src"
SRC_DIR="$OVERLAY_ROOT/main/typescript"
OUT_DIR="$DIST_ROOT/hydra-kernel/src/main/typescript"

if [ ! -d "$SRC_DIR" ]; then
    echo "error: missing source dir $SRC_DIR" >&2
    exit 1
fi

mkdir -p "$OUT_DIR/hydra"

# Write a minimal package.json marking the dist tree as ESM. Without this,
# tsc walks up to /Users/<you>/package.json (or wherever it finds the first
# package.json without a "type" field) and decides the dist files are
# CommonJS — which then rejects `import.meta.url` (TS1470) and forces
# `.ts` → `.js` rewrites. Hand-written test runtime depends on ESM.
cat > "$DIST_ROOT/hydra-kernel/package.json" <<'EOF'
{
  "name": "hydra-kernel-dist",
  "private": true,
  "type": "module"
}
EOF

# Merge the entire overlay main tree onto the generated kernel dist. Trailing
# /. on the source copies CONTENTS into the dest, leaving generated siblings
# (e.g. hydra/core.ts, the rest of hydra/core/*) untouched. `runtime.ts` lives
# alongside the GENERATED `core.ts` — they used to share the name and the
# hand-written file clobbered the generated kernel core every sync, masking
# all kernel exports (Term, Type, Literal, …) at tsc-check time. Now
# `runtime.ts` provides the JS-native value constructors (Given/None/
# Left/Right/Pair/Unit + Name/Namespace factories), while generated `core.ts`
# provides the kernel type definitions imported by every other generated file.
cp -R "$SRC_DIR/." "$OUT_DIR/"

echo "  Copied hand-written TypeScript runtime into $OUT_DIR/hydra/"

# Test-tree hand-written modules (testEnv.ts, jsonBindings.ts) — copied if the
# test source dir exists. testEnv mirrors the role of the Python/Java/Scala/
# Lisp hand-written equivalents: the DSL declares the FQNs so the coder can
# resolve references during inference, but the actual runtime values are
# provided per-language at test time. Full-tree merge (see the main-tree copy
# above for why); the source lives under hydra/core/test/ (#729), not hydra/test/.
TEST_SRC_DIR="$OVERLAY_ROOT/test/typescript"
TEST_OUT_DIR="$DIST_ROOT/hydra-kernel/src/test/typescript"
if [ -d "$TEST_SRC_DIR" ]; then
    mkdir -p "$TEST_OUT_DIR"
    cp -R "$TEST_SRC_DIR/." "$TEST_OUT_DIR/"
    echo "  Copied hand-written TypeScript test runtime into $TEST_OUT_DIR/hydra/"
fi
