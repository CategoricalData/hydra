#!/bin/bash
set -e

# Script to synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Java artifacts from the Hydra sources:
#   1. Main modules, eval lib, and coder modules (from JSON)
#   2. Kernel test modules (from JSON)
#   3. Generation tests (from Haskell DSL)
#
# Prerequisites:
#   - JSON modules must be up to date (run sync-haskell.sh and sync-ext.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-java.sh          # Full sync (all steps)
#   ./bin/sync-java.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-java.sh --help   # Show this help

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
            echo "Synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Java tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate Java modules and tests from JSON"
            echo "  3. Run Java build and tests (unless --quick)"
            echo "  4. Report new files to git add"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Hydra-Java"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT_DIR/hydra-java"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "Step 1/3: Building executable..."
echo ""
stack build hydra-ext:exe:bootstrap-from-json

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Step 2/3: Generating Java modules and tests from JSON..."
echo ""
stack exec bootstrap-from-json -- --target java --include-coders --include-tests --include-gentests $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Java generation failed"
    exit 1
fi

echo ""
echo "=========================================="
echo "Generation complete!"
echo "=========================================="

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 3/3: Building and testing Java..."
    echo ""

    cd "$HYDRA_ROOT_DIR"

    # First compile to catch build errors (these should fail the script)
    ./gradlew :hydra-java:compileJava :hydra-java:compileTestJava

    if [ $? -ne 0 ]; then
        echo ""
        echo "ERROR: Java compilation failed"
        exit 1
    fi

    # Run tests - use || true to prevent set -e from exiting on test failures
    # We handle the exit code manually below
    ./gradlew :hydra-java:test || TEST_RESULT=$?
    TEST_RESULT=${TEST_RESULT:-0}

    if [ $TEST_RESULT -eq 0 ]; then
        echo ""
        echo "All tests passed!"
    else
        echo ""
        echo "WARNING: Some tests failed (exit code $TEST_RESULT). Please review the output above."
    fi

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

cd "$HYDRA_JAVA_DIR"

# Find untracked Java files in gen directories
NEW_FILES=$(git status --porcelain src/gen-main/java src/gen-test/java 2>/dev/null | grep "^??" | awk '{print $2}')

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_JAVA_DIR"
    echo "  git add src/gen-main/java src/gen-test/java"
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
