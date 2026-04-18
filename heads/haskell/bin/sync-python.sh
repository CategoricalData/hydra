#!/usr/bin/env bash
set -euo pipefail

# Script to synchronize Hydra-Python with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Python artifacts from the Hydra sources:
#   1. Main modules, eval lib, and coder modules (from JSON)
#   2. Kernel test modules (from JSON)
#   3. Generation tests (from Haskell DSL)
#
# Prerequisites:
#   - JSON modules must be up to date (run sync-haskell.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-python.sh          # Full sync (all steps)
#   ./bin/sync-python.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-python.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/../.." && pwd )"
HYDRA_PYTHON_DIR="$HYDRA_ROOT_DIR/packages/hydra-python"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

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
            echo "  2. Generate Python main modules and tests from JSON"
            echo "  3. Generate ext Python modules into hydra-ext"
            echo "  4. Generate ext Python modules into hydra-python"
            echo "  5. Run Python tests (unless --quick)"
            echo "  6. Report new files to git add"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

banner2 "Synchronizing Hydra-Python"
echo ""

cd "$HYDRA_EXT_DIR"

TOTAL_STEPS=5

step 1 $TOTAL_STEPS "Building executable"
echo ""
stack build hydra:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Generating Python main modules and tests from JSON"
echo ""
# --output ../../dist/python: routing fans modules out into dist/python/<pkg>/
# based on namespace (kernel classes -> hydra-kernel, ext -> hydra-ext, etc.).
stack exec bootstrap-from-json -- --target python --output "../../dist/python" --include-coders --include-ext --include-dsls --include-tests $RTS_FLAGS || \
    warn "Python test generation had errors (some polymorphic types not supported). Continuing..."

# Copy hand-written test_env.py from heads/ into dist/. The patched test_graph.py
# below imports `hydra.test.test_env`, which must resolve under the dist tree at
# test time; the canonical source lives under heads/python/.
TEST_ENV_SRC="$HYDRA_ROOT_DIR/heads/python/src/test/python/hydra/test/test_env.py"
TEST_ENV_DST="$HYDRA_ROOT_DIR/dist/python/hydra-kernel/src/test/python/hydra/test/test_env.py"
if [ -f "$TEST_ENV_SRC" ]; then
    echo "  Copying test_env.py from heads/ into dist/..."
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

# Patch test_graph.py to replace empty test_graph/test_context with lazy versions via test_env
TESTGRAPH="../../dist/python/hydra-kernel/src/test/python/hydra/test/test_graph.py"
if [ -f "$TESTGRAPH" ]; then
    echo "  Post-processing: patching test_graph.py..."
    # Remove the module-level test_context and test_graph assignments so __getattr__ can intercept
    sed_inplace '/^test_context = /d' "$TESTGRAPH"
    sed_inplace '/^test_graph = /d' "$TESTGRAPH"
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

step 3 $TOTAL_STEPS "Generating hydra-pg and hydra-rdf Python modules from JSON"
echo ""
# The pg and rdf packages are loaded via --ext-only; with routing, their
# content lands under dist/python/hydra-pg/ and dist/python/hydra-rdf/.
stack exec bootstrap-from-json -- --target python --output "../../dist/python" --include-coders --ext-only $RTS_FLAGS

if [ "$QUICK_MODE" = false ]; then
    step 4 $TOTAL_STEPS "Running Python tests"
    echo ""

    # Run pytest from heads/python where pyproject.toml lives
    cd "$HYDRA_ROOT_DIR/heads/python"
    uv run pytest -q

    cd "$HYDRA_EXT_DIR"
else
    step 4 $TOTAL_STEPS "Skipped (--quick mode)"
fi

step 5 $TOTAL_STEPS "Checking for new files"
echo ""

DIST_PYTHON_DIR="$HYDRA_ROOT_DIR/dist/python/hydra-kernel"
cd "$DIST_PYTHON_DIR"

NEW_FILES=$(git status --porcelain src/main/python src/test/python 2>/dev/null | grep "^??" | awk '{print $2}' || true)

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $DIST_PYTHON_DIR"
    echo "  git add src/main/python src/test/python"
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

banner2_done "Hydra-Python sync complete!"
