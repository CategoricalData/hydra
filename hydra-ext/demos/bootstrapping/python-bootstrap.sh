#!/bin/bash
# Python bootstrapping demo: loads Hydra modules from JSON and generates code.
# Demonstrates that Python can independently load and process Hydra modules
# from a language-independent JSON representation.
#
# Usage: ./python-bootstrap.sh --target <haskell|java|python>

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_PYTHON_DIR="$HYDRA_ROOT/hydra-python"

echo "=========================================="
echo "Python Bootstrapping Demo"
echo "=========================================="
echo ""

# Use the project venv Python 3.12
PYTHON="$HYDRA_PYTHON_DIR/.venv/bin/python3"
if [ ! -f "$PYTHON" ]; then
    echo "Error: Python venv not found at $PYTHON"
    echo "Run: cd $HYDRA_PYTHON_DIR && python3.12 -m venv .venv"
    exit 1
fi

PYTHONPATH="$HYDRA_PYTHON_DIR/src/main/python:$HYDRA_PYTHON_DIR/src/gen-main/python"
export PYTHONPATH

# Run the Python bootstrap
"$PYTHON" -m hydra.bootstrap "$@" --json-dir "$HYDRA_ROOT/hydra-haskell/src/gen-main/json"
