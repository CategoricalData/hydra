#!/bin/bash
# Build and test a Python bootstrap target directory.
#
# Usage: ./test-python-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

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

echo "Running Python tests..."
cd "$OUTPUT_DIR"
TEST_DIRS="src/test/python"
if [ -d "src/gen-test/python/generation" ]; then
    TEST_DIRS="$TEST_DIRS src/gen-test/python/generation"
fi
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" pytest $TEST_DIRS 2>&1
