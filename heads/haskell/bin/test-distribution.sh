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
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

echo "=== Testing Haskell distribution: $PACKAGE ==="
echo "  (Note: hydra.cabal is monolithic today — running the full test suite)"
echo ""

cd "$HYDRA_HASKELL_DIR"
stack test
