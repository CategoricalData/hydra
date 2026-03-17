#!/bin/bash
# Python bootstrapping demo: loads Hydra modules from JSON and generates code.
# Demonstrates that Python can independently load and process Hydra modules
# from a language-independent JSON representation.
#
# Usage: ./invoke-python-host.sh --target <haskell|java|python> [--include-tests] [--kernel-only] [--types-only] [--output <dir>]
#
# The detailed step-by-step output (timing, file counts, etc.) is provided
# by the Python bootstrap module itself.

set -eo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
HYDRA_PYTHON_DIR="$HYDRA_ROOT/hydra-python"

# Locate Python interpreter
# PyPy3 is strongly recommended for term-level generation (CPython is too slow).
# For type-only generation, CPython works fine.
if command -v pypy3 >/dev/null 2>&1; then
    PYTHON="pypy3"
elif [ -f "$HYDRA_PYTHON_DIR/.venv-pypy/bin/pypy3" ]; then
    PYTHON="$HYDRA_PYTHON_DIR/.venv-pypy/bin/pypy3"
elif [ -f "$HYDRA_PYTHON_DIR/.venv/bin/python3" ]; then
    PYTHON="$HYDRA_PYTHON_DIR/.venv/bin/python3"
else
    echo "Error: No Python interpreter found."
    echo "Install pypy3 or create a venv: cd $HYDRA_PYTHON_DIR && python3.12 -m venv .venv"
    exit 1
fi

echo "Python: $($PYTHON --version 2>&1)"
echo ""

PYTHONPATH="$HYDRA_PYTHON_DIR/src/main/python:$HYDRA_PYTHON_DIR/src/gen-main/python"
export PYTHONPATH

# Run the Python bootstrap (its output includes detailed timing and file counts)
"$PYTHON" -m hydra.bootstrap "$@" --json-dir "$HYDRA_ROOT/hydra-haskell/src/gen-main/json"
