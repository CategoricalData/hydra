#!/usr/bin/env bash
# Layer 1 transform: Haskell DSL -> JSON, scoped to a single package.
#
# Usage:
#   transform-haskell-dsl-to-json.sh <pkg> [main|test] [--dist-json-root <dir>]
#
# Writes <pkg>'s <source-set> modules to
# <dist-json-root>/<pkg>/src/<set>/json/. Source set defaults to 'main'.
# If <pkg> has no modules in the requested source set (e.g. test for any
# package other than hydra-kernel today), the script exits 0 cleanly.
#
# This is a Layer 1 transform per feature_290_packaging-plan.md:
# stateless, idempotent, single (package, source-set) pair per invocation.
# Composition is the orchestrator's job.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [main|test] [--dist-json-root <dir>]" >&2
    echo "" >&2
    echo "Packages: hydra-kernel, hydra-haskell, hydra-java, hydra-python," >&2
    echo "          hydra-scala, hydra-lisp, hydra-pg, hydra-rdf." >&2
    exit 1
fi

PACKAGE="$1"
shift

SOURCE_SET="main"
if [ $# -gt 0 ] && [[ "$1" != --* ]]; then
    SOURCE_SET="$1"
    shift
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_HASKELL_DIR"

# Ensure the exec is built (fast no-op if already up to date).
stack build hydra:exe:transform-haskell-dsl-to-json >/dev/null 2>&1

stack exec transform-haskell-dsl-to-json -- \
    --package "$PACKAGE" \
    --source-set "$SOURCE_SET" \
    "$@"
