#!/bin/bash
set -eo pipefail

# Script to synchronize Hydra-Python with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Python artifacts from the Hydra sources:
#   1. Main modules, eval lib, and coder modules (from JSON)
#   2. Kernel test modules (from JSON)
#   3. Generation tests (from Haskell DSL)
#
# Prerequisites:
#   - JSON modules must be up to date (run sync-haskell.sh and sync-ext.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-python.sh          # Full sync (all steps)
#   ./bin/sync-python.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-python.sh --help   # Show this help

QUICK_MODE=false

for arg in "$@"; do
    case $arg in
        --quick)
            QUICK_MODE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Synchronize Hydra-Python with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Python tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate Python modules and tests from JSON"
            echo "  3. Run Python tests (unless --quick)"
            echo "  4. Report new files to git add"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Hydra-Python"
echo "=========================================="
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_PYTHON_DIR="$( cd "$HYDRA_EXT_DIR/../hydra-python" && pwd )"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "Step 1/4: Building executable..."
echo ""
stack build hydra-ext:exe:bootstrap-from-json

echo ""
echo "Step 2/4: Generating Python main modules and tests from JSON..."
echo ""
stack exec bootstrap-from-json -- --target python --include-coders --include-dsls --include-tests $RTS_FLAGS || {
    echo "WARNING: Python test generation had errors (some polymorphic types not supported). Continuing..."
}

# Patch test_graph.py to replace empty test_graph/test_context with lazy versions via test_env
echo "Patching test_graph.py..."
TESTGRAPH="../hydra-python/src/gen-test/python/hydra/test/test_graph.py"
if [ -f "$TESTGRAPH" ]; then
    # Remove the module-level test_context and test_graph assignments so __getattr__ can intercept
    sed -i '' '/^test_context = /d' "$TESTGRAPH"
    sed -i '' '/^test_graph = /d' "$TESTGRAPH"
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
    elif name == "test_context":
        if _test_context_cache is None:
            import hydra.test.test_env as _test_env
            _test_context_cache = _test_env.test_context()
        return _test_context_cache
    raise AttributeError(f"module 'hydra.test.test_graph' has no attribute {name!r}")
PYEOF
fi

echo ""
echo "Step 3/4: Generating ext Python demo modules from JSON..."
echo ""
stack exec bootstrap-from-json -- --target python --output . --include-coders --ext-only $RTS_FLAGS

echo ""
echo "=========================================="
echo "Generation complete!"
echo "=========================================="

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 4/4: Running Python tests..."
    echo ""

    cd "$HYDRA_PYTHON_DIR"

    # Activate virtual environment if it exists
    if [ -f ".venv/bin/activate" ]; then
        source .venv/bin/activate
    fi

    # Run pytest with PYTHONPATH set (kernel tests + generation tests)
    PYTHONPATH=src/main/python:src/gen-main/python:src/gen-test/python pytest src/test/python/test_suite_runner.py src/gen-test/python/generation -q

    cd "$HYDRA_EXT_DIR"
else
    echo ""
    echo "Step 4/4: Skipped (--quick mode)"
fi

echo ""
echo "=========================================="
echo "Checking for new files..."
echo "=========================================="
echo ""

cd "$HYDRA_PYTHON_DIR"

# Find untracked Python files in gen directories
NEW_FILES=$(git status --porcelain src/main/python src/gen-main/python src/gen-test/python 2>/dev/null | grep "^??" | awk '{print $2}' || true)

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_PYTHON_DIR"
    echo "  git add src/main/python src/gen-main/python src/gen-test/python"
    echo ""
    echo "New files:"
    echo "$NEW_FILES" | head -20
    NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
    if [ "$NEW_COUNT" -gt 20 ]; then
        echo "  ... and $((NEW_COUNT - 20)) more"
    fi
else
    echo "No new files created."
fi

echo ""
echo "=========================================="
echo "Sync complete!"
echo "=========================================="
