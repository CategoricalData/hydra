#!/bin/bash
# Bootstrap Hydra to Haskell from JSON modules.
# Generates code into a standalone /tmp directory, copies static resources,
# then builds and runs the Haskell test suite.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/haskell-to-haskell"
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "=========================================="
echo "Bootstrap: Haskell -> JSON -> Haskell"
echo "=========================================="
echo ""
echo "Output directory: $OUTPUT_DIR"
echo ""

# Step 1: Ensure JSON is up to date
echo "Step 1: Ensuring JSON exports are up to date..."
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
echo ""

# Step 2: Clean output directory
echo "Step 2: Preparing output directory..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
echo "  Cleaned: $OUTPUT_DIR"
echo ""

# Step 3: Bootstrap from JSON (generate code to /tmp)
echo "Step 3: Generating Haskell code from JSON..."
cd "$HYDRA_EXT_DIR"
stack exec bootstrap-from-json -- --target haskell --output "$OUTPUT_BASE" $RTS_FLAGS
echo ""

# Step 4: Copy static resources
echo "Step 4: Copying static resources..."

# Build files
echo "  Copying build files..."
cp "$HYDRA_HASKELL_DIR/stack.yaml" "$OUTPUT_DIR/"
cp "$HYDRA_HASKELL_DIR/stack.yaml.lock" "$OUTPUT_DIR/" 2>/dev/null || true
cp "$HYDRA_HASKELL_DIR/package.yaml" "$OUTPUT_DIR/"

# Hand-written primitive library implementations
echo "  Copying primitive library implementations..."
mkdir -p "$OUTPUT_DIR/src/main/haskell/Hydra/Lib"
cp "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra/Lib/"*.hs "$OUTPUT_DIR/src/main/haskell/Hydra/Lib/"

# Core hand-written modules
echo "  Copying core hand-written modules..."
cp "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra/Settings.hs" "$OUTPUT_DIR/src/main/haskell/Hydra/"
cp "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra/Minimal.hs" "$OUTPUT_DIR/src/main/haskell/Hydra/"
cp "$HYDRA_HASKELL_DIR/src/main/haskell/Hydra/Kernel.hs" "$OUTPUT_DIR/src/main/haskell/Hydra/"

# Test infrastructure
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/haskell"
cp "$HYDRA_HASKELL_DIR/src/test/haskell/Spec.hs" "$OUTPUT_DIR/src/test/haskell/"
cp "$HYDRA_HASKELL_DIR/src/test/haskell/GenerationSpec.hs" "$OUTPUT_DIR/src/test/haskell/"
echo ""

# Summary
echo "Step 5: Summary of generated files..."
MAIN_COUNT=$(find "$OUTPUT_DIR/src/gen-main" -name "*.hs" 2>/dev/null | wc -l | tr -d ' ')
TEST_COUNT=$(find "$OUTPUT_DIR/src/gen-test" -name "*.hs" 2>/dev/null | wc -l | tr -d ' ')
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.hs" 2>/dev/null | wc -l | tr -d ' ')
echo "  Generated main modules:   $MAIN_COUNT files"
echo "  Generated test modules:   $TEST_COUNT files"
echo "  Static resources:         $STATIC_COUNT files"
echo ""

echo "=========================================="
echo "Bootstrap to Haskell: COMPLETE"
echo "Output: $OUTPUT_DIR"
echo "=========================================="
