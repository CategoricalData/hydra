#!/bin/bash
# Bootstrap Hydra to Python from JSON modules (via Python host).
#
# Usage: ./python-to-python.sh [--output DIR] [--types-only] [--kernel-only]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"

# Collect extra flags
EXTRA_FLAGS=""
while [ $# -gt 0 ]; do
    case "$1" in
        --output) OUTPUT_BASE="$2"; shift ;;
        --output=*) OUTPUT_BASE="${1#--output=}" ;;
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $1" ;;
    esac
    shift
done
OUTPUT_DIR="$OUTPUT_BASE/python-to-python"

echo "=========================================="
echo "Python to Python bootstrapping demo"
echo "=========================================="
echo ""
echo "  Host language:   Python"
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

# Step 2: Set up target directory (clean + copy static files)
"$SCRIPT_DIR/setup-python-target.sh" "$OUTPUT_DIR"

# Step 3: Generate code
echo "Step 3: Generating Python code (via Python host)..."
STEP_START=$(date +%s)
"$SCRIPT_DIR/invoke-python-host.sh" --target python --output "$OUTPUT_BASE" --include-tests $EXTRA_FLAGS 2>&1
STEP_END=$(date +%s)
echo "  Time: $((STEP_END - STEP_START))s"
echo ""

# Step 4: Build and test
"$SCRIPT_DIR/test-python-target.sh" "$OUTPUT_DIR"
TEST_EXIT=$?

TOTAL_END=$(date +%s)
TOTAL_SECS=$((TOTAL_END - TOTAL_START))

echo "=========================================="
if [ $TEST_EXIT -eq 0 ]; then
    echo "Bootstrap Python-to-Python: COMPLETE (tests PASSED)"
else
    echo "Bootstrap Python-to-Python: COMPLETE (tests FAILED)"
fi
echo "  Output:     $OUTPUT_DIR"
echo "  Total time: ${TOTAL_SECS}s"
echo "=========================================="

exit $TEST_EXIT
