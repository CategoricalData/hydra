#!/bin/bash
# Clean output directory and copy static resources for a Python bootstrap target.
# This is host-language-independent: the same static resources are needed
# regardless of which host (Haskell, Java, Python) generated the code.
#
# Usage: ./setup-python-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_PYTHON_DIR="$HYDRA_ROOT/hydra-python"
PYTHON_RESOURCES="$SCRIPT_DIR/resources/python"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Python target..."

# Build/config files
echo "  Copying build files..."
cp "$PYTHON_RESOURCES/pyproject.toml" "$OUTPUT_DIR/"
cp "$PYTHON_RESOURCES/README.md" "$OUTPUT_DIR/"

# Hand-written source files
echo "  Copying hand-written source files..."
PY_SRC="$HYDRA_PYTHON_DIR/src/main/python/hydra"
PY_DST="$OUTPUT_DIR/src/main/python/hydra"
mkdir -p "$PY_DST"

for f in __init__.py tools.py py.typed generation.py bootstrap.py; do
    cp "$PY_SRC/$f" "$PY_DST/" 2>/dev/null || true
done

for d in lib dsl sources; do
    if [ -d "$PY_SRC/$d" ]; then
        cp -r "$PY_SRC/$d" "$PY_DST/"
        find "$PY_DST/$d" -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
    fi
done

# Copy ext modules from baseline.
# Test infrastructure (e.g. test_suite_runner.py) imports ext modules.
# Copy all ext from baseline to ensure they're available.
echo "  Copying ext modules from baseline..."
PY_GEN="$OUTPUT_DIR/src/gen-main/python"
PY_BASELINE="$HYDRA_PYTHON_DIR/src/gen-main/python"
if [ -d "$PY_BASELINE/hydra/ext" ]; then
    mkdir -p "$PY_GEN/hydra"
    rm -rf "$PY_GEN/hydra/ext"
    cp -r "$PY_BASELINE/hydra/ext" "$PY_GEN/hydra/"
    # Ensure __init__.py files exist in ext directories
    find "$PY_GEN/hydra/ext" -type d -not -name "__pycache__" -not -path "*/__pycache__/*" | while read dir; do
        if [ ! -f "$dir/__init__.py" ]; then
            echo "from pkgutil import extend_path" > "$dir/__init__.py"
            echo "__path__ = extend_path(__path__, __name__)" >> "$dir/__init__.py"
        fi
    done
    echo "    Copied hydra/ext from baseline"
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

# Create symlink to hydra-haskell so that relative paths (../hydra-haskell/...)
# used by test_suite_runner.py to find JSON modules resolve correctly.
echo "  Creating hydra-haskell symlink..."
if [ ! -e "$OUTPUT_DIR/../hydra-haskell" ]; then
    ln -s "$HYDRA_ROOT/hydra-haskell" "$OUTPUT_DIR/../hydra-haskell"
    echo "    Symlinked ../hydra-haskell -> $HYDRA_ROOT/hydra-haskell"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.py" ! -name "__init__.py" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""
