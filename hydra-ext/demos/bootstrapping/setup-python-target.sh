#!/bin/bash
# Copy static resources and run tests for a Python bootstrap target directory.
# This is host-language-independent: the same static resources are needed
# regardless of which host (Haskell, Java, Python) generated the code.
#
# Usage: ./setup-python-target.sh <output-dir>
#
# The output directory should already contain src/gen-main/python/ with generated code.

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_PYTHON_DIR="$HYDRA_ROOT/hydra-python"

# Create namespace package __init__.py files in generated directories.
echo "Creating namespace package __init__.py files..."
for gen_dir in "$OUTPUT_DIR/src/gen-main/python" "$OUTPUT_DIR/src/gen-test/python"; do
    if [ -d "$gen_dir" ]; then
        find "$gen_dir" -type d -not -name "__pycache__" -not -path "*/__pycache__/*" | while read dir; do
            if [ "$dir" = "$gen_dir" ]; then continue; fi
            if [ ! -f "$dir/__init__.py" ]; then
                echo "from pkgutil import extend_path" > "$dir/__init__.py"
                echo "__path__ = extend_path(__path__, __name__)" >> "$dir/__init__.py"
            fi
        done
    fi
done

# Step: Copy static resources
echo "Copying static resources for Python target..."

# Build/config files
echo "  Copying build configuration..."
cp "$HYDRA_PYTHON_DIR/pyproject.toml" "$OUTPUT_DIR/"
cp "$HYDRA_PYTHON_DIR/pyrightconfig.json" "$OUTPUT_DIR/" 2>/dev/null || true
cp "$HYDRA_PYTHON_DIR/uv.lock" "$OUTPUT_DIR/" 2>/dev/null || true
touch "$OUTPUT_DIR/README.md"

# Hand-written source files
echo "  Copying hand-written source files..."
PY_SRC="$HYDRA_PYTHON_DIR/src/main/python/hydra"
PY_DST="$OUTPUT_DIR/src/main/python/hydra"
mkdir -p "$PY_DST"

for f in __init__.py tools.py py.typed; do
    cp "$PY_SRC/$f" "$PY_DST/" 2>/dev/null || true
done

for d in lib dsl sources; do
    if [ -d "$PY_SRC/$d" ]; then
        cp -r "$PY_SRC/$d" "$PY_DST/"
        find "$PY_DST/$d" -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
    fi
done

# Kernel sources modules (hydra.sources.*) needed for evaluation tests.
echo "  Copying kernel sources modules..."
if [ -d "$HYDRA_PYTHON_DIR/src/gen-main/python/hydra/sources" ]; then
    mkdir -p "$OUTPUT_DIR/src/gen-main/python/hydra/sources"
    cp -r "$HYDRA_PYTHON_DIR/src/gen-main/python/hydra/sources/"* "$OUTPUT_DIR/src/gen-main/python/hydra/sources/"
    find "$OUTPUT_DIR/src/gen-main/python/hydra/sources" -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
fi

# Test infrastructure
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/python"
cp "$HYDRA_PYTHON_DIR/src/test/python/conftest.py" "$OUTPUT_DIR/src/test/python/" 2>/dev/null || true
for f in test_suite_runner.py test_python.py test_generated_code.py test_grammar.py test_json.py test_lazy_flow_evaluation.py kernel_bindings.py test_summary_report.py; do
    if [ -f "$HYDRA_PYTHON_DIR/src/test/python/$f" ]; then
        cp "$HYDRA_PYTHON_DIR/src/test/python/$f" "$OUTPUT_DIR/src/test/python/"
    fi
done

# Summary
MAIN_COUNT=$(find "$OUTPUT_DIR/src/gen-main" -name "*.py" 2>/dev/null | wc -l | tr -d ' ')
TEST_COUNT=$(find "$OUTPUT_DIR/src/gen-test" -name "*.py" 2>/dev/null | wc -l | tr -d ' ')
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.py" 2>/dev/null | wc -l | tr -d ' ')
echo "  Generated main modules:   $MAIN_COUNT files"
echo "  Generated test modules:   $TEST_COUNT files"
echo "  Static resources:         $STATIC_COUNT files"
echo ""

# Run tests
echo "Running Python tests..."
STEP_START=$(date +%s)
cd "$OUTPUT_DIR"
pytest src/test/ src/gen-test/ 2>&1
TEST_EXIT=$?
STEP_END=$(date +%s)
echo "  Test time: $((STEP_END - STEP_START))s"
echo ""

exit $TEST_EXIT
