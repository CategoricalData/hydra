#!/bin/bash
# Bootstrap Hydra to Python from JSON modules (via Haskell host).
# Generates code into a standalone /tmp directory, copies static resources,
# then runs the Python test suite.
#
# Usage: ./bootstrap-to-python.sh [--types-only] [--kernel-only]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/haskell-to-python"
RTS_FLAGS="+RTS -K256M -A32M -RTS"

# Collect extra flags
EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $arg" ;;
    esac
done

echo "=========================================="
echo "Bootstrap: Haskell -> JSON -> Python"
echo "=========================================="
echo ""
echo "  Host language:   Haskell"
echo "  Target language: Python"
echo "  Output:          $OUTPUT_DIR"
if [ -n "$EXTRA_FLAGS" ]; then
    echo "  Flags:          $EXTRA_FLAGS"
fi
echo ""

TOTAL_START=$(date +%s)

# Step 1: Verify JSON modules exist
echo "Step 1: Verifying JSON modules..."
JSON_DIR="$HYDRA_HASKELL_DIR/src/gen-main/json"
JSON_COUNT=$(find "$JSON_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
if [ "$JSON_COUNT" -eq 0 ]; then
    echo "  ERROR: No JSON files found in $JSON_DIR"
    echo "  Ensure the repository is synced. See bin/sync-haskell.sh."
    exit 1
fi
echo "  JSON input files: $JSON_COUNT (in $JSON_DIR)"
echo ""

# Step 2: Clean output directory
echo "Step 2: Preparing output directory..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
echo "  Cleaned: $OUTPUT_DIR"
echo ""

# Step 3: Bootstrap from JSON (generate code to /tmp)
echo "Step 3: Generating Python code from JSON..."
STEP_START=$(date +%s)
cd "$HYDRA_EXT_DIR"
stack exec bootstrap-from-json -- --target python --output "$OUTPUT_BASE" $EXTRA_FLAGS $RTS_FLAGS
STEP_END=$(date +%s)
echo "  Time: $((STEP_END - STEP_START))s"
echo ""

# Steps 4-6: Copy static resources and run tests
echo "Steps 4-6: Setting up target and running tests..."
"$SCRIPT_DIR/setup-python-target.sh" "$OUTPUT_DIR"
TEST_EXIT=$?

TOTAL_END=$(date +%s)
TOTAL_SECS=$((TOTAL_END - TOTAL_START))

echo "=========================================="
if [ $TEST_EXIT -eq 0 ]; then
    echo "Bootstrap to Python: COMPLETE (tests PASSED)"
else
    echo "Bootstrap to Python: COMPLETE (tests FAILED)"
fi
echo "  Output:     $OUTPUT_DIR"
echo "  Total time: ${TOTAL_SECS}s"
echo "=========================================="

exit $TEST_EXIT
