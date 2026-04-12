#!/usr/bin/env bash
set -euo pipefail

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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT_DIR/packages/hydra-haskell"
HYDRA_HASKELL_HEAD_DIR="$HYDRA_ROOT_DIR/heads/haskell"

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
            echo "Synchronize Hydra-Haskell generated code with the source of truth."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Haskell tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate Haskell modules and tests from JSON"
            echo "  3. Build and test Haskell (unless --quick)"
            echo "  4. Report new files to git add"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

banner2 "Synchronizing Hydra-Haskell (from JSON)"
echo ""

cd "$HYDRA_EXT_DIR"

TOTAL_STEPS=4

step 1 $TOTAL_STEPS "Building executable"
echo ""
stack build hydra:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Generating Haskell modules and tests from JSON"
echo ""
stack exec bootstrap-from-json -- --target haskell --output ../hydra-haskell --include-tests --include-gentests $RTS_FLAGS

if [ "$QUICK_MODE" = false ]; then
    step 3 $TOTAL_STEPS "Building and testing Haskell"
    echo ""

    cd "$HYDRA_HASKELL_HEAD_DIR"

    stack build 2>&1
    stack test 2>&1

    cd "$HYDRA_EXT_DIR"
else
    step 3 $TOTAL_STEPS "Skipped (--quick mode)"
fi

step 4 $TOTAL_STEPS "Checking for new files"
echo ""

cd "$HYDRA_HASKELL_DIR"

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

banner2_done "Hydra-Haskell sync complete!"
