#!/bin/bash
# Generate code using the Emacs Lisp host implementation.
#
# Prerequisites:
#   - Emacs 27+ must be installed (for built-in JSON support)
#
# Usage: ./invoke-emacs-lisp-host.sh --target <lang> --output <dir> [OPTIONS]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
HYDRA_EL_DIR="$HYDRA_ROOT/hydra-lisp/hydra-emacs-lisp"

# Parse arguments (pass through to EL bootstrap)
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
    JSON_DIR="$HYDRA_ROOT/hydra-haskell/src/gen-main/json"
    EXTRA_ARGS="$EXTRA_ARGS --json-dir $JSON_DIR"
fi

echo "Emacs Lisp host: generating $TARGET code..."
echo "  Target:    $TARGET"
echo ""

# Ensure coder modules are available
CODER_CHECK=""
case "$TARGET" in
    haskell)                               CODER_CHECK="$HYDRA_EL_DIR/src/gen-main/emacs-lisp/hydra/ext/haskell/coder.el" ;;
    java)                                  CODER_CHECK="$HYDRA_EL_DIR/src/gen-main/emacs-lisp/hydra/ext/java/coder.el" ;;
    python)                                CODER_CHECK="$HYDRA_EL_DIR/src/gen-main/emacs-lisp/hydra/ext/python/coder.el" ;;
    clojure|scheme|common-lisp|emacs-lisp) CODER_CHECK="$HYDRA_EL_DIR/src/gen-main/emacs-lisp/hydra/ext/lisp/coder.el" ;;
esac

if [ -n "$CODER_CHECK" ] && [ ! -f "$CODER_CHECK" ]; then
    echo "  Coder modules not found. Generating from ext JSON..."
    cd "$HYDRA_ROOT/hydra-ext"
    stack build bootstrap-from-json 2>&1 | grep -v "^$"
    EXT_JSON_DIR="$HYDRA_ROOT/hydra-ext/src/gen-main/json"
    stack exec bootstrap-from-json -- \
        --target emacs-lisp \
        --output "$HYDRA_EL_DIR" \
        --include-coders \
        --ext-json-dir "$EXT_JSON_DIR" \
        --json-dir "$JSON_DIR" 2>&1
    echo "  Coder modules generated."
    echo ""
fi

# Run the EL bootstrap (increase stack size to avoid segfault during code gen)
cd "$HYDRA_EL_DIR"
ulimit -s 65536 2>/dev/null || true
emacs --batch --no-init-file \
     -l src/main/emacs-lisp/hydra/bootstrap.el \
     -- $EXTRA_ARGS
