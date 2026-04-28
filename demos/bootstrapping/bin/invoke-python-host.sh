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
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_PYTHON_DIR="$HYDRA_ROOT/heads/python"

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

# Ensure every coder/ext package referenced by the Python host has a
# Python distribution. The PYTHONPATH below lists each dist/python/hydra-*
# tree, but missing dirs result in ImportError when the bootstrap loads
# coders for non-language packages (pg, rdf, etc.). sync-default()
# doesn't generate non-language packages, so on a fresh checkout some of
# these may be missing. Assemble them on demand — warm-cache short-
# circuits in seconds. Redirect output to a log file so the assembler's
# per-package "Done: N main..." lines don't confuse the bootstrap-all
# log parser.
ASSEMBLE_LOG="${ASSEMBLE_LOG:-/tmp/hydra-python-host-coder-assembly.log}"
: > "$ASSEMBLE_LOG"
for coder_pkg in hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp hydra-pg hydra-rdf; do
    coder_base="$HYDRA_ROOT/dist/python/$coder_pkg/src/main/python"
    if [ ! -d "$coder_base/hydra" ]; then
        echo "Python host needs $coder_pkg; generating (see $ASSEMBLE_LOG)..."
        (cd "$HYDRA_ROOT" && heads/python/bin/assemble-distribution.sh "$coder_pkg") >> "$ASSEMBLE_LOG" 2>&1
    fi
done

PYTHONPATH="$HYDRA_PYTHON_DIR/src/main/python"
for pkg in hydra-kernel hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp hydra-pg hydra-rdf hydra-ext; do
    PYTHONPATH="$PYTHONPATH:$HYDRA_ROOT/dist/python/$pkg/src/main/python"
done
export PYTHONPATH
export HYDRA_JSON_DIR="$HYDRA_ROOT/dist/json"

# Run the Python bootstrap (its output includes detailed timing and file counts).
# --json-dir now points at the dist/json root; the bootstrap walks per-package
# subdirectories in dependency order.
"$PYTHON" -m hydra.bootstrap "$@" --json-dir "$HYDRA_ROOT/dist/json"
