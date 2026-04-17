#!/usr/bin/env bash
# Layer 1 transform: JSON -> target language, scoped to a single package.
#
# Usage:
#   transform-json-to-target.sh <target> <pkg> [main|test] [--output <dir>]
#                               [--dist-json-root <dir>] [OPTIONS]
#
# Runs the Haskell bootstrap-from-json exec to generate <target>-language
# sources for <pkg>'s modules. The full per-package JSON universe is loaded
# (so cross-package type references resolve), but only <pkg>'s modules are
# actually written to <output>.
#
# Target must be one of: haskell, java, python, scala, clojure, scheme,
#                        common-lisp, emacs-lisp.
#
# Per-target convenience wrappers (transform-json-to-{haskell,java,...}.sh)
# forward to this script.
#
# This is a Layer 1 transform per feature_290_packaging-plan.md:
# stateless, single (package, source-set) pair per invocation.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <target> <package> [main|test] [OPTIONS]" >&2
    echo "" >&2
    echo "Targets:  haskell, java, python, scala, clojure, scheme," >&2
    echo "          common-lisp, emacs-lisp" >&2
    echo "Packages: hydra-kernel, hydra-haskell, hydra-java, hydra-python," >&2
    echo "          hydra-scala, hydra-lisp, hydra-pg, hydra-rdf" >&2
    echo "" >&2
    echo "Extra args (e.g. --output, --include-tests) are forwarded to" >&2
    echo "bootstrap-from-json." >&2
    exit 1
fi

TARGET="$1"
PACKAGE="$2"
shift 2

SOURCE_SET="main"
if [ $# -gt 0 ] && [[ "$1" != --* ]]; then
    SOURCE_SET="$1"
    shift
fi

# --source-set test adds --include-tests; main is the default.
TEST_FLAG=""
if [ "$SOURCE_SET" = "test" ]; then
    TEST_FLAG="--include-tests"
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_HASKELL_DIR"

# Ensure the exec is built (fast no-op if already up to date).
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1

# Choose load flags based on which package we're scoping to:
#   - Baseline (hydra-kernel/hydra-haskell): always loaded.
#   - Coder packages (hydra-java/python/scala/lisp): loaded via --include-coders.
#   - Ext packages (hydra-pg/hydra-rdf):           loaded via --ext-only.
# The --package <pkg> flag then narrows modsToGenerate to just that package.
case "$PACKAGE" in
    hydra-kernel|hydra-haskell)
        LOAD_FLAGS=""
        ;;
    hydra-java|hydra-python|hydra-scala|hydra-lisp)
        LOAD_FLAGS="--include-coders"
        ;;
    hydra-pg|hydra-rdf)
        # --ext-only loads the pg+rdf packages. --include-coders is also
        # included so coder-package types (referenced by pg/rdf modules) are
        # resolvable in the universe.
        LOAD_FLAGS="--include-coders --ext-only"
        ;;
    *)
        echo "Warning: unknown package '$PACKAGE'; using default load flags." >&2
        LOAD_FLAGS="--include-coders"
        ;;
esac

stack exec bootstrap-from-json -- \
    --target "$TARGET" \
    $LOAD_FLAGS \
    --package "$PACKAGE" \
    $TEST_FLAG \
    "$@"
