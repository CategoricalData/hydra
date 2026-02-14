#!/bin/bash
# Bootstrap Hydra to Python from JSON modules.
# Generates code into a standalone /tmp directory, copies static resources,
# then runs the Python test suite.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HYDRA_PYTHON_DIR="$HYDRA_ROOT/hydra-python"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/haskell-to-python"
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "=========================================="
echo "Bootstrap: Haskell -> JSON -> Python"
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
echo "Step 3: Generating Python code from JSON..."
cd "$HYDRA_EXT_DIR"
stack exec bootstrap-from-json -- --target python --output "$OUTPUT_BASE" $RTS_FLAGS
echo ""

# Step 4: Copy static resources
echo "Step 4: Copying static resources..."

# Build/config files
echo "  Copying build configuration..."
cp "$HYDRA_PYTHON_DIR/pyproject.toml" "$OUTPUT_DIR/"
cp "$HYDRA_PYTHON_DIR/pyrightconfig.json" "$OUTPUT_DIR/" 2>/dev/null || true
cp "$HYDRA_PYTHON_DIR/uv.lock" "$OUTPUT_DIR/" 2>/dev/null || true

# Hand-written primitive library implementations
echo "  Copying primitive library implementations..."
mkdir -p "$OUTPUT_DIR/src/main/python/hydra/lib"
cp "$HYDRA_PYTHON_DIR/src/main/python/hydra/lib/"*.py "$OUTPUT_DIR/src/main/python/hydra/lib/"
cp "$HYDRA_PYTHON_DIR/src/main/python/hydra/lib/py.typed" "$OUTPUT_DIR/src/main/python/hydra/lib/" 2>/dev/null || true

# Core hand-written modules
echo "  Copying core hand-written modules..."
cp "$HYDRA_PYTHON_DIR/src/main/python/hydra/__init__.py" "$OUTPUT_DIR/src/main/python/hydra/"
cp "$HYDRA_PYTHON_DIR/src/main/python/hydra/tools.py" "$OUTPUT_DIR/src/main/python/hydra/" 2>/dev/null || true

# Test infrastructure
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/python"
cp "$HYDRA_PYTHON_DIR/src/test/python/conftest.py" "$OUTPUT_DIR/src/test/python/" 2>/dev/null || true
for f in test_suite_runner.py test_python.py test_generated_code.py test_grammar.py test_json.py test_lazy_flow_evaluation.py kernel_bindings.py test_summary_report.py; do
  if [ -f "$HYDRA_PYTHON_DIR/src/test/python/$f" ]; then
    cp "$HYDRA_PYTHON_DIR/src/test/python/$f" "$OUTPUT_DIR/src/test/python/"
  fi
done
echo ""

# Summary
echo "Step 5: Summary of generated files..."
MAIN_COUNT=$(find "$OUTPUT_DIR/src/gen-main" -name "*.py" 2>/dev/null | wc -l | tr -d ' ')
TEST_COUNT=$(find "$OUTPUT_DIR/src/gen-test" -name "*.py" 2>/dev/null | wc -l | tr -d ' ')
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.py" 2>/dev/null | wc -l | tr -d ' ')
echo "  Generated main modules:   $MAIN_COUNT files"
echo "  Generated test modules:   $TEST_COUNT files"
echo "  Static resources:         $STATIC_COUNT files"
echo ""

echo "=========================================="
echo "Bootstrap to Python: COMPLETE"
echo "Output: $OUTPUT_DIR"
echo "=========================================="
