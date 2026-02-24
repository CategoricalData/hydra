#!/bin/bash
# Bootstrap Hydra to Java from JSON modules (via Java host).
# Generates code into a standalone /tmp directory, copies static resources,
# then builds and runs the Java test suite.
#
# Usage: ./java-to-java.sh [--types-only] [--kernel-only]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/java-to-java"

# Collect extra flags
EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $arg" ;;
    esac
done

echo "=========================================="
echo "Java to Java bootstrapping demo"
echo "=========================================="
echo ""
echo "  Host language:   Java"
echo "  Target language: Java"
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
    echo "  Run bin/update-json-kernel.sh first to generate JSON modules."
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

# Step 3: Bootstrap from JSON via Java host
echo "Step 3: Mapping JSON to Java (via Java host)..."
STEP_START=$(date +%s)
"$SCRIPT_DIR/java-bootstrap.sh" --target java --output "$OUTPUT_BASE" $EXTRA_FLAGS 2>&1
STEP_END=$(date +%s)
echo "  Time: $((STEP_END - STEP_START))s"
echo ""

# Steps 4-6: Copy static resources and run tests
echo "Steps 4-6: Setting up target and running tests..."
"$SCRIPT_DIR/setup-java-target.sh" "$OUTPUT_DIR"
TEST_EXIT=$?

TOTAL_END=$(date +%s)
TOTAL_SECS=$((TOTAL_END - TOTAL_START))

echo "=========================================="
if [ $TEST_EXIT -eq 0 ]; then
    echo "Bootstrap Java-to-Java: COMPLETE (tests PASSED)"
else
    echo "Bootstrap Java-to-Java: COMPLETE (tests FAILED)"
fi
echo "  Output:     $OUTPUT_DIR"
echo "  Total time: ${TOTAL_SECS}s"
echo "=========================================="

exit $TEST_EXIT
