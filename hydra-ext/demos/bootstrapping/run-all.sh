#!/bin/bash
# Run all Java bootstrapping demos: Java-to-Haskell, Java-to-Java, Java-to-Python.
#
# This script:
#   1. Regenerates kernel and test JSON from the Haskell host (once)
#   2. Runs each java-to-xxx bootstrap path sequentially
#
# Usage: ./run-all.sh [--skip-json] [--types-only] [--kernel-only]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"

SKIP_JSON=false
EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --skip-json) SKIP_JSON=true ;;
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $arg" ;;
    esac
done

echo "=========================================="
echo "Hydra Bootstrapping Demo — All Paths"
echo "=========================================="
echo ""

TOTAL_START=$(date +%s)

# Step 0: Generate JSON from the Haskell host (once)
if [ "$SKIP_JSON" = true ]; then
    echo "Step 0: Skipping JSON generation (--skip-json)"
    JSON_DIR="$HYDRA_HASKELL_DIR/src/gen-main/json"
    JSON_COUNT=$(find "$JSON_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
    echo "  Using existing JSON: $JSON_COUNT files in $JSON_DIR"
else
    echo "Step 0: Generating JSON from Haskell host..."
    STEP_START=$(date +%s)

    echo "  Building hydra-haskell..."
    cd "$HYDRA_HASKELL_DIR"
    stack build 2>&1 | tail -3
    echo "  Exporting main modules to JSON..."
    stack exec update-json-main -- +RTS -K256M -A32M -RTS
    echo "  Exporting test modules to JSON..."
    stack exec update-json-test -- +RTS -K256M -A32M -RTS

    echo "  Building hydra-ext..."
    cd "$HYDRA_EXT_DIR"
    stack build 2>&1 | tail -3
    echo "  Exporting ext modules to JSON..."
    stack exec update-json-main -- +RTS -K256M -A32M -RTS

    STEP_END=$(date +%s)
    JSON_DIR="$HYDRA_HASKELL_DIR/src/gen-main/json"
    JSON_COUNT=$(find "$JSON_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
    echo "  JSON files: $JSON_COUNT (in $JSON_DIR)"
    echo "  Time: $((STEP_END - STEP_START))s"
fi
echo ""

# Run each bootstrap path
RESULTS=()

for TARGET in haskell java python; do
    echo "=========================================="
    echo "Running: java-to-$TARGET"
    echo "=========================================="
    STEP_START=$(date +%s)

    if "$SCRIPT_DIR/java-to-$TARGET.sh" $EXTRA_FLAGS; then
        STEP_END=$(date +%s)
        RESULTS+=("java-to-$TARGET: PASSED ($((STEP_END - STEP_START))s)")
    else
        STEP_END=$(date +%s)
        RESULTS+=("java-to-$TARGET: FAILED ($((STEP_END - STEP_START))s)")
    fi
    echo ""
done

TOTAL_END=$(date +%s)

echo ""
echo "=========================================="
echo "Bootstrapping Demo — Summary"
echo "=========================================="
for result in "${RESULTS[@]}"; do
    echo "  $result"
done
echo ""
echo "  Total time: $((TOTAL_END - TOTAL_START))s"
echo "=========================================="
