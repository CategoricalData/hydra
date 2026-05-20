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
        # Cross-package self-containment: each dist/typescript/<pkg>/.../hydra/
        # imports kernel modules via relative paths (../lib/maps.js, ../names.js,
        # etc.). They resolve to kernel-pkg-local paths but the kernel sources
        # only physically exist under dist/typescript/hydra-kernel/. Symlink
        # every kernel file into this package so the imports resolve at runtime
        # under node's NodeNext module resolution. Mirrors how Java's gradle
        # source-set crossover and Python's pyright extraPaths bridge the two
        # trees at compile time.
        PKG_HYDRA="$HYDRA_ROOT/dist/typescript/$PKG/src/main/typescript/hydra"
        KERNEL_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra"
        if [ -d "$PKG_HYDRA" ] && [ -d "$KERNEL_HYDRA" ]; then
            # Walk all kernel .ts files (including subdirs: show/, validate/,
            # decode/, encode/, error/, json/, extract/, lib/, lib/defaults/, ...).
            while IFS= read -r kfile; do
                rel="${kfile#$KERNEL_HYDRA/}"
                dest="$PKG_HYDRA/$rel"
                # Only link if the package didn't generate its own file at that path.
                if [ ! -e "$dest" ]; then
                    mkdir -p "$(dirname "$dest")"
                    ln -sf "$kfile" "$dest"
                fi
            done < <(find "$KERNEL_HYDRA" -name "*.ts" -not -path "*/node_modules/*")
        fi
        ;;
esac
