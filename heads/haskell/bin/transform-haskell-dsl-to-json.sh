#!/usr/bin/env bash
# Layer 1 transform: Haskell DSL -> JSON.
#
# Usage:
#   transform-haskell-dsl-to-json.sh <pkg> [main|test] [--dist-json-root <dir>]
#   transform-haskell-dsl-to-json.sh --all [main|test] [--dist-json-root <dir>]
#
# Per-package form writes <pkg>'s <source-set> modules to
# <dist-json-root>/<pkg>/src/<set>/json/. Source set defaults to 'main'.
# If <pkg> has no modules in the requested source set, the script exits 0
# cleanly.
#
# Batch (--all) form loads the Haskell universe once and routes every
# module to its owning package's per-package JSON tree. Much faster than
# invoking this script 12 times because the Haskell startup + DSL
# compilation happens just once.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [main|test] [--dist-json-root <dir>]" >&2
    echo "       $0 --all      [main|test] [--dist-json-root <dir>]" >&2
    echo "" >&2
    echo "Packages: hydra-kernel, hydra-haskell, hydra-java, hydra-python," >&2
    echo "          hydra-scala, hydra-lisp, hydra-coq, hydra-javascript," >&2
    echo "          hydra-wasm, hydra-pg, hydra-rdf, hydra-ext." >&2
    exit 1
fi

if [ "$1" = "--all" ]; then
    MODE="all"
    shift
else
    MODE="single"
    PACKAGE="$1"
    shift
fi

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

if [ "$MODE" = "all" ]; then
    stack exec transform-haskell-dsl-to-json -- \
        --all \
        --source-set "$SOURCE_SET" \
        "$@"
else
    stack exec transform-haskell-dsl-to-json -- \
        --package "$PACKAGE" \
        --source-set "$SOURCE_SET" \
        "$@"
fi
