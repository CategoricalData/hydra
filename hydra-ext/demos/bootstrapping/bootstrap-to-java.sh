#!/bin/bash
# Bootstrap Hydra to Java from JSON modules (via Haskell host).
# Generates code into a standalone /tmp directory, copies static resources,
# then builds and runs the Java test suite.
#
# Usage: ./bootstrap-to-java.sh [--types-only] [--kernel-only]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/haskell-to-java"
RTS_FLAGS="+RTS -K512M -A64M -RTS"

# Collect extra flags
EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $arg" ;;
    esac
done

echo "=========================================="
echo "Bootstrap: Haskell -> JSON -> Java"
echo "=========================================="
echo ""
echo "  Host language:   Haskell"
echo "  Target language: Java"
echo "  Output:          $OUTPUT_DIR"
if [ -n "$EXTRA_FLAGS" ]; then
    echo "  Flags:          $EXTRA_FLAGS"
fi
echo ""

TOTAL_START=$(date +%s)

# Step 1: Ensure JSON is up to date
echo "Step 1: Ensuring JSON exports are up to date..."
STEP_START=$(date +%s)
echo "  Building hydra-haskell..."
cd "$HYDRA_HASKELL_DIR"
stack build 2>&1 | tail -3
echo "  Exporting main modules to JSON..."
stack exec update-json-main -- $RTS_FLAGS
echo "  Exporting test modules to JSON..."
stack exec update-json-test -- $RTS_FLAGS
echo "  Building hydra-ext..."
cd "$HYDRA_EXT_DIR"
stack build 2>&1 | tail -3
echo "  Exporting ext modules to JSON..."
stack exec update-json-main -- $RTS_FLAGS
STEP_END=$(date +%s)
echo "  Time: $((STEP_END - STEP_START))s"
echo ""

# Count JSON input files
JSON_DIR="$HYDRA_HASKELL_DIR/src/gen-main/json"
JSON_COUNT=$(find "$JSON_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
echo "  JSON input files: $JSON_COUNT (in $JSON_DIR)"
echo ""

# Step 2: Clean output directory
echo "Step 2: Preparing output directory..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
echo "  Cleaned: $OUTPUT_DIR"
echo ""

# Step 3: Bootstrap from JSON (generate code to /tmp)
echo "Step 3: Generating Java code from JSON..."
STEP_START=$(date +%s)
cd "$HYDRA_EXT_DIR"
stack exec bootstrap-from-json -- --target java --output "$OUTPUT_BASE" $EXTRA_FLAGS $RTS_FLAGS
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
    echo "Bootstrap to Java: COMPLETE (tests PASSED)"
else
    echo "Bootstrap to Java: COMPLETE (tests FAILED)"
fi
echo "  Output:     $OUTPUT_DIR"
echo "  Total time: ${TOTAL_SECS}s"
echo "=========================================="

exit $TEST_EXIT
