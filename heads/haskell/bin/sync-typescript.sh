#!/usr/bin/env bash
set -euo pipefail

# Script to synchronize Hydra-TypeScript with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all TypeScript artifacts from the Hydra sources:
#   1. Main modules and coder modules (from JSON)
#   2. Kernel test modules (from JSON)
#
# Prerequisites:
#   - JSON modules must be up to date (run sync-haskell.sh and sync-ext.sh first)
#   - Run from the hydra-ext directory (or the script will cd there)
#
# Usage:
#   ./bin/sync-typescript.sh          # Full sync (all steps)
#   ./bin/sync-typescript.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-typescript.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/../.." && pwd )"
HYDRA_TYPESCRIPT_DIR="$HYDRA_ROOT_DIR/heads/typescript"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

QUICK_MODE=false

for arg in "$@"; do
    case $arg in
        --quick)
            QUICK_MODE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Synchronize Hydra-TypeScript with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running TypeScript tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate TypeScript main modules and tests from JSON"
            echo "  3. Generate ext TypeScript modules into hydra-ext"
            echo "  4. Run TypeScript tests (unless --quick)"
            echo "  5. Report new files to git add"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

banner2 "Synchronizing Hydra-TypeScript"
echo ""

cd "$HYDRA_EXT_DIR"

TOTAL_STEPS=5
DIST_HYDRA="$HYDRA_ROOT_DIR/dist/typescript/hydra-kernel/src/main/typescript/hydra"
HEADS_HYDRA="$HYDRA_TYPESCRIPT_DIR/src/main/typescript/hydra"

step 1 $TOTAL_STEPS "Building executable"
echo ""
stack build hydra:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Generating TypeScript main modules and tests from JSON"
echo ""
stack exec bootstrap-from-json -- --target typescript --include-coders --include-tests $RTS_FLAGS || \
    warn "TypeScript generation had errors. Continuing..."

step 3 $TOTAL_STEPS "Generating ext TypeScript modules into dist/typescript/hydra-ext from JSON"
echo ""
stack exec bootstrap-from-json -- --target typescript --output "../../dist/typescript/hydra-ext" --include-coders --ext-only $RTS_FLAGS

step 4 $TOTAL_STEPS "Copying hand-written runtime into generated tree"
echo ""
# Copy hand-written lib/ files (primitive implementations) that aren't generated
for f in "$HEADS_HYDRA"/lib/*.ts; do
    fname=$(basename "$f")
    if [ ! -f "$DIST_HYDRA/lib/$fname" ]; then
        cp "$f" "$DIST_HYDRA/lib/$fname"
        echo "  Copied lib/$fname"
    fi
done
# Copy hand-written modules that don't conflict with generated ones
for f in compute.ts libraries.ts index.ts; do
    if [ ! -f "$DIST_HYDRA/$f" ]; then
        cp "$HEADS_HYDRA/$f" "$DIST_HYDRA/$f"
        echo "  Copied $f"
    fi
done
# Copy dsl/ directory (not generated)
if [ -d "$HEADS_HYDRA/dsl" ] && [ ! -d "$DIST_HYDRA/dsl" ]; then
    cp -r "$HEADS_HYDRA/dsl" "$DIST_HYDRA/dsl"
    echo "  Copied dsl/"
fi
# Copy tools/ directory (not generated)
if [ -d "$HEADS_HYDRA/tools" ] && [ ! -d "$DIST_HYDRA/tools" ]; then
    cp -r "$HEADS_HYDRA/tools" "$DIST_HYDRA/tools"
    echo "  Copied tools/"
fi

if [ "$QUICK_MODE" = false ]; then
    step 5 $TOTAL_STEPS "Running TypeScript tests"
    echo ""

    cd "$HYDRA_TYPESCRIPT_DIR"
    pnpm test

    cd "$HYDRA_EXT_DIR"
else
    step 5 $TOTAL_STEPS "Skipped (--quick mode)"
fi

banner2_done "Hydra-TypeScript sync complete!"
