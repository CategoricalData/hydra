#!/usr/bin/env bash
# Layer 2.5 tester: run Haskell tests against an already-assembled
# distribution.
#
# Usage:
#   test-distribution.sh <pkg>
#
# Invokes `stack test` from packages/hydra-haskell. The current hydra.cabal
# is monolithic and covers every package's dist, so per-package test
# scoping is limited; today this runs the full test suite regardless of
# which package is requested. Per-package test targets are a followup
# (requires splitting hydra.cabal into per-package cabal files).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi

PACKAGE="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
THIS_SCRIPT="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

echo "=== Testing Haskell distribution: $PACKAGE ==="
echo "  (Note: hydra.cabal is monolithic today — running the full test suite)"
echo ""

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check haskell "$HYDRA_ROOT_DIR/dist/haskell" "$HYDRA_HASKELL_DIR/src/test" "$THIS_SCRIPT"; then
    echo "  Cache hit: no changes since last successful Haskell test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi

cd "$HYDRA_HASKELL_DIR"
stack test

test_cache_record haskell "$HYDRA_ROOT_DIR/dist/haskell" "$HYDRA_HASKELL_DIR/src/test" "$THIS_SCRIPT"
