#!/bin/bash
# Generate code using the Scheme host implementation.
#
# Prerequisites:
#   - Guile 3.x must be installed
#
# Usage: ./invoke-scheme-host.sh --target <lang> --output <dir> [OPTIONS]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_SCHEME_DIR="$HYDRA_ROOT/heads/lisp/scheme"

# Parse arguments (pass through to Scheme bootstrap)
TARGET=""
EXTRA_ARGS=""
JSON_DIR=""

while [ $# -gt 0 ]; do
    case "$1" in
        --target) TARGET="$2"; EXTRA_ARGS="$EXTRA_ARGS --target $2"; shift ;;
        --output) EXTRA_ARGS="$EXTRA_ARGS --output $2"; shift ;;
        --json-dir) JSON_DIR="$2"; EXTRA_ARGS="$EXTRA_ARGS --json-dir $2"; shift ;;
        *) EXTRA_ARGS="$EXTRA_ARGS $1" ;;
    esac
    shift
done

if [ -z "$TARGET" ]; then
    echo "Usage: $0 --target <lang> --output <dir> [OPTIONS]"
    exit 1
fi

if [ -z "$JSON_DIR" ]; then
    # The Scheme bootstrap still takes a single kernel JSON directory.
    # Full per-package walking (see issue #290 Phase 1c) is not yet implemented.
    JSON_DIR="$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json"
    EXTRA_ARGS="$EXTRA_ARGS --json-dir $JSON_DIR"
fi

echo "Scheme host: generating $TARGET code..."
echo "  Target:    $TARGET"
echo ""

# Ensure coder modules are available
CODER_CHECK=""
case "$TARGET" in
    haskell)                               CODER_CHECK="$HYDRA_ROOT/dist/scheme/hydra-kernel/src/main/scheme/hydra/haskell/coder.scm" ;;
    java)                                  CODER_CHECK="$HYDRA_ROOT/dist/scheme/hydra-kernel/src/main/scheme/hydra/java/coder.scm" ;;
    python)                                CODER_CHECK="$HYDRA_ROOT/dist/scheme/hydra-kernel/src/main/scheme/hydra/python/coder.scm" ;;
    clojure|scheme|common-lisp|emacs-lisp) CODER_CHECK="$HYDRA_ROOT/dist/scheme/hydra-kernel/src/main/scheme/hydra/lisp/coder.scm" ;;
esac

if [ -n "$CODER_CHECK" ] && [ ! -f "$CODER_CHECK" ]; then
    echo "  Coder modules not found. Generating from per-package JSON..."
    cd "$HYDRA_ROOT/heads/haskell"
    stack build hydra:exe:bootstrap-from-json 2>&1 | grep -v "^$"
    stack exec bootstrap-from-json -- \
        --target scheme \
        --output "$HYDRA_SCHEME_DIR" \
        --include-coders \
        --dist-json-root "$HYDRA_ROOT/dist/json" 2>&1
    echo "  Coder modules generated."
    echo ""
fi

# Run the Scheme bootstrap
cd "$HYDRA_SCHEME_DIR"
guile --no-auto-compile \
     -s src/main/scheme/hydra/bootstrap.scm \
     -- $EXTRA_ARGS
