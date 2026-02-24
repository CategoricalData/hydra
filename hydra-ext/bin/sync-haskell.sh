#!/bin/bash
set -eo pipefail

# Script to synchronize Hydra-Haskell generated code with the source of truth.
#
# This script regenerates all Haskell artifacts from the Hydra sources:
#   1. Main modules and eval lib (from JSON)
#   2. Kernel test modules (from JSON)
#   3. Generation tests (from Haskell DSL)
#
# Prerequisites:
#   - JSON modules must be up to date (run the JSON generation scripts first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-haskell.sh          # Full sync (all steps)
#   ./bin/sync-haskell.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-haskell.sh --help   # Show this help

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
            echo "Synchronize Hydra-Haskell generated code with the source of truth."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Haskell tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate Haskell modules and tests from JSON"
            echo "  3. Run Haskell build and tests (unless --quick)"
            echo "  4. Report new files to git add"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Hydra-Haskell"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_HASKELL_DIR="$( cd "$HYDRA_EXT_DIR/../hydra-haskell" && pwd )"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "Step 1/3: Building executable..."
echo ""
stack build hydra-ext:exe:bootstrap-from-json

echo ""
echo "Step 2/3: Generating Haskell modules and tests from JSON..."
echo ""
stack exec bootstrap-from-json -- --target haskell --output ../hydra-haskell --include-tests --include-gentests $RTS_FLAGS

echo ""
echo "=========================================="
echo "Generation complete!"
echo "=========================================="

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 3/3: Building and testing Haskell..."
    echo ""

    cd "$HYDRA_HASKELL_DIR"

    stack build 2>&1
    stack test 2>&1

    cd "$HYDRA_EXT_DIR"
else
    echo ""
    echo "Step 3/3: Skipped (--quick mode)"
fi

echo ""
echo "=========================================="
echo "Checking for new files..."
echo "=========================================="
echo ""

cd "$HYDRA_HASKELL_DIR"

# Find untracked Haskell files in gen directories
NEW_FILES=$(git status --porcelain src/gen-main/haskell src/gen-test/haskell 2>/dev/null | grep "^??" | awk '{print $2}' || true)

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_HASKELL_DIR"
    echo "  git add src/gen-main/haskell src/gen-test/haskell"
    echo ""
    echo "New files:"
    echo "$NEW_FILES" | head -20
    NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
    if [ "$NEW_COUNT" -gt 20 ]; then
        echo "  ... and $((NEW_COUNT - 20)) more"
    fi
else
    echo "No new files created."
fi

echo ""
echo "=========================================="
echo "Sync complete!"
echo "=========================================="
