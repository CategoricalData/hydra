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

# Patch test_graph.py to use test_env (real graph with primitives) instead of emptyGraph.
# TODO: Replace this with hydra.test.environment module.
echo "Patching test_graph.py..."
TESTGRAPH="$OUTPUT_DIR/src/gen-test/python/hydra/test/test_graph.py"
if [ -f "$TESTGRAPH" ]; then
    sed -i '' '/^test_graph = /d' "$TESTGRAPH"
    sed -i '' '/^test_context = /d' "$TESTGRAPH"
    cat >> "$TESTGRAPH" << 'PYEOF'

_test_graph_cache = None
_test_context_cache = None

def __getattr__(name):
    global _test_graph_cache, _test_context_cache
    if name == "test_graph":
        if _test_graph_cache is None:
            import hydra.test.test_env as _test_env
            _test_graph_cache = _test_env.test_graph()
        return _test_graph_cache
    if name == "test_context":
        if _test_context_cache is None:
            import hydra.context
            import hydra.lib.maps
            _test_context_cache = hydra.context.Context([], [], hydra.lib.maps.empty())
        return _test_context_cache
    raise AttributeError(f"module 'hydra.test.test_graph' has no attribute {name!r}")
PYEOF
fi

echo "Running Python tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" pytest src/test/python 2>&1
