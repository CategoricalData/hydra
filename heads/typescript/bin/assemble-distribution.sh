#!/usr/bin/env bash
# assemble-distribution.sh — TypeScript distribution assembler.
#
# Called by bin/sync.sh during Phase 3 (target=typescript). Generates
# `<pkg>` into dist/typescript/<pkg>/src/main/typescript/ and, for the
# kernel, copies the hand-written TS runtime alongside.
#
# Usage:
#   assemble-distribution.sh <pkg>
#
# Currently the kernel is the only meaningful target; other packages
# (hydra-pg, hydra-rdf) can be added incrementally once their runtime
# bindings are written.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "usage: assemble-distribution.sh <pkg>" >&2
    exit 2
fi

PKG="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

case "$PKG" in
    hydra-kernel)
        echo "  Generating $PKG -> typescript (main + test)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" test \
            --output "$HYDRA_ROOT/dist/typescript"
        "$HYDRA_TS_HEAD/bin/copy-kernel-runtime.sh" \
            --dist-root "$HYDRA_ROOT/dist/typescript"
        ;;
    hydra-pg|hydra-rdf|hydra-coq|hydra-wasm|hydra-ext)
        echo "  skipping: $PKG -> typescript (not yet supported)"
        ;;
    *)
        echo "  Generating $PKG -> typescript (main)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        ;;
esac
