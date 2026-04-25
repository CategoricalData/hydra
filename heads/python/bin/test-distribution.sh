#!/usr/bin/env bash
# Layer 2.5 tester: run Python tests against an already-assembled distribution.
#
# Usage:
#   test-distribution.sh <pkg>
#
# Invokes pytest from heads/python (where pyproject.toml lives). Today
# pytest runs the full suite regardless of <pkg>; per-package test scoping
# requires per-package pyproject.toml files (future work).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi

PACKAGE="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
THIS_SCRIPT="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
HYDRA_PYTHON_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_PYTHON_HEAD/../.." && pwd )"

echo "=== Testing Python distribution: $PACKAGE ==="
echo "  (Note: pyproject.toml is monolithic today — running the full test suite)"
echo ""

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check python "$HYDRA_ROOT_DIR/dist/python" "$HYDRA_PYTHON_HEAD/src/test" "$THIS_SCRIPT"; then
    echo "  Cache hit: no changes since last successful Python test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi

cd "$HYDRA_PYTHON_HEAD"
uv run pytest -q

test_cache_record python "$HYDRA_ROOT_DIR/dist/python" "$HYDRA_PYTHON_HEAD/src/test" "$THIS_SCRIPT"
