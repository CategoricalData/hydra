#!/bin/bash
set -e

# Script to regenerate all Haskell artifacts from Hydra sources and run tests.
#
# This is the Haskell equivalent of hydra-ext/bin/sync-java.sh and sync-python.sh.
# It ensures hydra-haskell is in a fully consistent state by regenerating all
# generated Haskell code in the correct order, with rebuilds between phases
# (since each phase writes .hs files that subsequent phases depend on).
#
# Phases:
#   1. Generate kernel modules (mainModules -> mainModules)
#   2. Generate kernel test modules (mainModules -> testModules)
#   3. Generate eval lib modules (mainModules -> evalLibModules)
#   4. Generate encoder/decoder source modules (kernelTypesModules)
#   5. Regenerate kernel modules (to pick up new encoder/decoder sources)
#   6. Generate generation tests
#   7. Export and verify JSON kernel
#   8. Run tests
#
# Prerequisites:
#   - Stack is installed and configured
#   - Run from the hydra-haskell directory (or the script will cd there)
#
# Usage:
#   ./bin/sync-haskell.sh          # Full sync (all steps including tests)
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
            echo "Regenerate all Haskell artifacts from Hydra sources."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Generate kernel modules"
            echo "  2. Generate kernel test modules"
            echo "  3. Generate eval lib modules"
            echo "  4. Generate encoder/decoder source modules"
            echo "  5. Regenerate kernel modules (picks up new encoder/decoder sources)"
            echo "  6. Generate generation tests"
            echo "  7. Export and verify JSON kernel"
            echo "  8. Run tests (unless --quick)"
            exit 0
            ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_HASKELL_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

TOTAL_STEPS=8
if [ "$QUICK_MODE" = true ]; then
    TOTAL_STEPS=7
fi

echo "=========================================="
echo "Synchronizing Hydra-Haskell"
echo "=========================================="
echo ""

# Phase 1: Generate kernel modules
echo "Step 1/$TOTAL_STEPS: Generating kernel modules..."
echo ""
stack build hydra:exe:update-haskell-kernel
stack exec update-haskell-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel generation failed"
    exit 1
fi

# Rebuild to pick up newly generated kernel files
echo ""
echo "Rebuilding..."
stack build

# Phase 2: Generate kernel test modules
echo ""
echo "Step 2/$TOTAL_STEPS: Generating kernel test modules..."
echo ""
stack build hydra:exe:update-kernel-tests
stack exec update-kernel-tests -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel test generation failed"
    exit 1
fi

# Rebuild to pick up newly generated test files
echo ""
echo "Rebuilding..."
stack build

# Phase 3: Generate eval lib modules
echo ""
echo "Step 3/$TOTAL_STEPS: Generating eval lib modules..."
echo ""
stack build hydra:exe:update-haskell-eval-lib
stack exec update-haskell-eval-lib -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Eval lib generation failed"
    exit 1
fi

# Rebuild to pick up newly generated eval lib files
echo ""
echo "Rebuilding..."
stack build

# Phase 4: Generate encoder/decoder source modules
echo ""
echo "Step 4/$TOTAL_STEPS: Generating encoder/decoder source modules..."
echo ""
stack build hydra:exe:update-haskell-sources
stack exec update-haskell-sources -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Encoder/decoder source generation failed"
    exit 1
fi

# Rebuild to pick up encoder/decoder source modules (they change the library)
echo ""
echo "Rebuilding..."
stack build

# Phase 5: Regenerate kernel modules (to be consistent with new encoder/decoder sources)
echo ""
echo "Step 5/$TOTAL_STEPS: Regenerating kernel modules (post encoder/decoder)..."
echo ""
stack exec update-haskell-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel regeneration failed"
    exit 1
fi

# Rebuild with the final kernel
echo ""
echo "Rebuilding..."
stack build

# Phase 6: Generate generation tests
echo ""
echo "Step 6/$TOTAL_STEPS: Generating generation tests..."
echo ""
stack build hydra:exe:update-generation-tests
stack exec update-generation-tests -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "WARNING: Some generation tests failed to generate"
fi

# Rebuild to pick up generated test files
echo ""
echo "Rebuilding..."
stack build

# Phase 7: Export and verify JSON kernel
echo ""
echo "Step 7/$TOTAL_STEPS: Exporting and verifying JSON kernel..."
echo ""
stack build hydra:exe:update-json-kernel hydra:exe:verify-json-kernel
stack exec update-json-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: JSON kernel export failed"
    exit 1
fi

stack exec verify-json-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: JSON kernel verification failed"
    exit 1
fi

echo ""
echo "=========================================="
echo "Generation complete!"
echo "=========================================="

# Phase 8: Run tests
if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 8/$TOTAL_STEPS: Running tests..."
    echo ""

    TEST_LOG="$HYDRA_HASKELL_DIR/test-output.log"
    stack test 2>&1 | tee "$TEST_LOG"
    TEST_RESULT=${PIPESTATUS[0]}

    if [ $TEST_RESULT -eq 0 ]; then
        echo ""
        echo "All tests passed!"
    else
        echo ""
        echo "WARNING: Some tests failed (exit code $TEST_RESULT). See $TEST_LOG"
    fi
else
    echo ""
    echo "Step 8/$TOTAL_STEPS: Skipped (--quick mode)"
fi

echo ""
echo "=========================================="
echo "Checking for new files..."
echo "=========================================="
echo ""

NEW_FILES=$(git status --porcelain src/gen-main/haskell src/gen-test/haskell src/gen-main/json 2>/dev/null | grep "^??" | awk '{print $2}')

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_HASKELL_DIR"
    echo "  git add src/gen-main/haskell src/gen-test/haskell src/gen-main/json"
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
