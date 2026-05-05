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
for gen_dir in "$OUTPUT_DIR/src/main/python" "$OUTPUT_DIR/src/test/python"; do
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

# test_graph.py post-generation patch removed. The DSL emits
# test_graph as @lru_cache(1) def test_graph() (lazy, complex term)
# and test_context as `test_context = test_env.test_context` (eager
# value reference). The hand-written test_env.py exposes both at the
# same arity expected by the DSL.

echo "Running Python tests..."
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" \
    HYDRA_JSON_DIR="${HYDRA_JSON_DIR:-$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json}" \
    pytest src/test/python 2>&1
