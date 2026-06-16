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
HYDRA_PYTHON_DIR="$HYDRA_ROOT/heads/python"

# Select a Python interpreter for the test runner. Bare `pytest` would let the
# pytest shebang choose the interpreter (often /usr/bin/python3, which on Debian
# 12 / Ubuntu 22.04 is 3.11), regardless of which `python3` the user shell
# resolves. The bootstrap output's pyproject.toml requires Python >=3.12, so we
# pin the interpreter explicitly and invoke pytest as a module. CPython is
# preferred here because PyPy may not yet support all 3.12+ syntax features.
# See GitHub issue #486.
if [ -n "${HYDRA_PYTHON_TEST_INTERPRETER:-}" ]; then
    PYTHON="$HYDRA_PYTHON_TEST_INTERPRETER"
elif [ -n "${HYDRA_PYTHON_INTERPRETER:-}" ]; then
    PYTHON="$HYDRA_PYTHON_INTERPRETER"
elif [ -x "$HYDRA_PYTHON_DIR/.venv/bin/python3" ]; then
    PYTHON="$HYDRA_PYTHON_DIR/.venv/bin/python3"
elif command -v python3 >/dev/null 2>&1; then
    PYTHON="python3"
else
    echo "Error: No Python interpreter found for the test runner."
    echo "Install Python >=3.12 or set HYDRA_PYTHON_TEST_INTERPRETER."
    exit 1
fi

PY_VERSION="$("$PYTHON" -c 'import sys; print("%d.%d" % sys.version_info[:2])' 2>/dev/null || true)"
if ! "$PYTHON" -c 'import sys; sys.exit(0 if sys.version_info >= (3, 12) else 1)' 2>/dev/null; then
    echo "Error: $PYTHON reports Python ${PY_VERSION:-unknown}, but the bootstrap"
    echo "output's pyproject.toml declares requires-python = \">=3.12\" (PEP 695 syntax)."
    echo "Set HYDRA_PYTHON_TEST_INTERPRETER to a Python >=3.12 binary. See #486."
    exit 1
fi
echo "Test interpreter: $PYTHON (Python $PY_VERSION)"

cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" \
    HYDRA_JSON_DIR="${HYDRA_JSON_DIR:-$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json}" \
    "$PYTHON" -m pytest src/test/python 2>&1
