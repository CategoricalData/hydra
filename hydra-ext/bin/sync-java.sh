#!/bin/bash
set -e

# Script to synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Java artifacts from the Hydra sources:
#   1. Kernel modules (hydra-java/src/gen-main/java/hydra/*)
#   2. Eval lib modules (hydra-java/src/gen-main/java/hydra/eval/lib/*)
#   3. Kernel tests (hydra-java/src/gen-test/java/hydra/test/*)
#   4. Generation tests (hydra-java/src/gen-test/java/generation/*)
#
# Prerequisites:
#   - Hydra-Haskell must be consistent (all Haskell artifacts regenerated, all tests passing)
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
            echo "  1. Build all executables"
            echo "  2. Generate Java kernel modules"
            echo "  3. Generate Java eval lib modules"
            echo "  4. Generate Java kernel tests"
            echo "  5. Generate Java generation tests"
            echo "  6. Run Java build and tests (unless --quick)"
            echo "  7. Report new files to git add"
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

echo "Step 1/6: Building executables..."
echo ""
stack build \
    hydra-ext:exe:update-java-kernel \
    hydra-ext:exe:update-java-eval-lib \
    hydra-ext:exe:update-java-kernel-tests \
    hydra-ext:exe:update-java-generation-tests

if [ $? -ne 0 ]; then
    echo "ERROR: Build failed"
    exit 1
fi

echo ""
echo "Step 2/6: Generating Java kernel modules..."
echo ""
stack exec update-java-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel generation failed"
    exit 1
fi

echo ""
echo "Step 3/6: Generating Java eval lib modules..."
echo ""
stack exec update-java-eval-lib -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Eval lib generation failed"
    exit 1
fi

echo ""
echo "Step 4/6: Generating Java kernel tests..."
echo ""
stack exec update-java-kernel-tests -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel tests generation failed"
    exit 1
fi

echo ""
echo "Step 5/6: Generating Java generation tests..."
echo ""
stack exec update-java-generation-tests -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "WARNING: Some generation tests failed to generate (this is expected for some modules)"
fi

echo ""
echo "=========================================="
echo "Generation complete!"
echo "=========================================="

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 6/6: Building and testing Java..."
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
        echo "Known failures: BigInt->BigFloat precision, String->Binary encoding, transcendental math precision"
    fi

    cd "$HYDRA_EXT_DIR"
else
    echo ""
    echo "Step 6/6: Skipped (--quick mode)"
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
