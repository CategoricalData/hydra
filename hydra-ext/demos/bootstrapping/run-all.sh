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

EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $arg" ;;
    esac
done

echo "=========================================="
echo "Hydra Bootstrapping Demo — All Paths"
echo "=========================================="
echo ""

TOTAL_START=$(date +%s)

# Step 0: Verify JSON modules exist
echo "Step 0: Verifying JSON modules..."
JSON_DIR="$HYDRA_HASKELL_DIR/src/gen-main/json"
JSON_COUNT=$(find "$JSON_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
if [ "$JSON_COUNT" -eq 0 ]; then
    echo "  ERROR: No JSON files found in $JSON_DIR"
    echo "  Ensure the repository is synced. See bin/sync-haskell.sh."
    exit 1
fi
echo "  JSON input files: $JSON_COUNT (in $JSON_DIR)"
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
