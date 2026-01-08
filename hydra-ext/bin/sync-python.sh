#!/bin/bash
set -e

# Script to synchronize Hydra-Python with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Python artifacts from the Hydra sources:
#   1. Kernel modules (hydra/kernel/*)
#   2. Kernel sources modules (hydra/sources/*) - Module AST as Python data
#   3. Eval lib modules (hydra/eval/lib/*)
#   4. Kernel tests (hydra/test/*)
#   5. Generation tests (generation/hydra/test/*)
#
# Prerequisites:
#   - Hydra-Haskell must be consistent (all Haskell artifacts regenerated, all tests passing)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-python.sh          # Full sync (all steps)
#   ./bin/sync-python.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-python.sh --help   # Show this help

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
            echo "Synchronize Hydra-Python with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Python tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build all executables"
            echo "  2. Generate Python kernel modules"
            echo "  3. Generate Python kernel sources modules"
            echo "  4. Generate Python eval lib modules"
            echo "  5. Generate Python kernel tests"
            echo "  6. Generate Python generation tests"
            echo "  7. Run Python tests (unless --quick)"
            echo "  8. Report new files to git add"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Hydra-Python"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_PYTHON_DIR="$( cd "$HYDRA_EXT_DIR/../hydra-python" && pwd )"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "Step 1/7: Building executables..."
echo ""
stack build \
    hydra-ext:exe:update-python-kernel \
    hydra-ext:exe:update-python-kernel-sources \
    hydra-ext:exe:update-python-eval-lib \
    hydra-ext:exe:update-python-kernel-tests \
    hydra-ext:exe:update-python-generation-tests

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Step 2/7: Generating Python kernel modules..."
echo ""
stack exec update-python-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel generation failed"
    exit 1
fi

echo ""
echo "Step 3/7: Generating Python kernel sources modules..."
echo ""
stack exec update-python-kernel-sources -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel sources generation failed"
    exit 1
fi

echo ""
echo "Step 4/7: Generating Python eval lib modules..."
echo ""
stack exec update-python-eval-lib -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Eval lib generation failed"
    exit 1
fi

echo ""
echo "Step 5/7: Generating Python kernel tests..."
echo ""
stack exec update-python-kernel-tests -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel tests generation failed"
    exit 1
fi

echo ""
echo "Step 6/7: Generating Python generation tests..."
echo ""
stack exec update-python-generation-tests -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Generation tests generation failed"
    exit 1
fi

echo ""
echo "=========================================="
echo "Generation complete!"
echo "=========================================="

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 7/7: Running Python tests..."
    echo ""

    cd "$HYDRA_PYTHON_DIR"

    # Activate virtual environment if it exists
    if [ -f ".venv/bin/activate" ]; then
        source .venv/bin/activate
    fi

    # Run pytest with PYTHONPATH set
    PYTHONPATH=src/main/python:src/gen-test/python pytest src/gen-test/python/generation -q

    if [ $? -eq 0 ]; then
        echo ""
        echo "All generation tests passed!"
    else
        echo ""
        echo "WARNING: Some tests failed. Please review the output above."
    fi

    cd "$HYDRA_EXT_DIR"
else
    echo ""
    echo "Step 7/7: Skipped (--quick mode)"
fi

echo ""
echo "=========================================="
echo "Checking for new files..."
echo "=========================================="
echo ""

cd "$HYDRA_PYTHON_DIR"

# Find untracked Python files in gen directories
NEW_FILES=$(git status --porcelain src/main/python src/gen-test/python 2>/dev/null | grep "^??" | awk '{print $2}')

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_PYTHON_DIR"
    echo "  git add src/main/python src/gen-test/python"
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
