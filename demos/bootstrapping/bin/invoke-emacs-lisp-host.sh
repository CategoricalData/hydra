#!/bin/bash
# Generate code using the Emacs Lisp host implementation.
#
# Prerequisites:
#   - Emacs 27+ must be installed (for built-in JSON support)
#
# Usage: ./invoke-emacs-lisp-host.sh --target <lang> --output <dir> [OPTIONS]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EL_DIR="$HYDRA_ROOT/heads/lisp/emacs-lisp"

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
    # The Emacs Lisp bootstrap still takes a single kernel JSON directory.
    # Full per-package walking (see issue #290 Phase 1c) is not yet implemented.
    JSON_DIR="$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json"
    EXTRA_ARGS="$EXTRA_ARGS --json-dir $JSON_DIR"
fi

echo "Emacs Lisp host: generating $TARGET code..."
echo "  Target:    $TARGET"
echo ""

# Ensure coder modules are available
CODER_CHECK=""
case "$TARGET" in
    haskell)                               CODER_CHECK="$HYDRA_ROOT/dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp/hydra/haskell/coder.el" ;;
    java)                                  CODER_CHECK="$HYDRA_ROOT/dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp/hydra/java/coder.el" ;;
    python)                                CODER_CHECK="$HYDRA_ROOT/dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp/hydra/python/coder.el" ;;
    clojure|scheme|common-lisp|emacs-lisp) CODER_CHECK="$HYDRA_ROOT/dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp/hydra/lisp/coder.el" ;;
esac

if [ -n "$CODER_CHECK" ] && [ ! -f "$CODER_CHECK" ]; then
    echo "  Coder modules not found. Generating from per-package JSON..."
    cd "$HYDRA_ROOT/heads/haskell"
    stack build hydra:exe:bootstrap-from-json
    stack exec bootstrap-from-json -- \
        --target emacs-lisp \
        --output "$HYDRA_ROOT/dist/emacs-lisp/hydra-kernel" \
        --include-coders \
        --dist-json-root "$HYDRA_ROOT/dist/json" 2>&1
    echo "  Coder modules generated."
    echo ""
fi

# Run the EL bootstrap
# Use --eval+load instead of -l to avoid byte-compiling the bootstrap file,
# which can overflow the bytecode compiler's C stack on deeply nested forms.
# Prefer Emacs 30+ with native compilation when available (5-10x faster).
EMACS_CMD="emacs"
if [ -x "/opt/homebrew/opt/emacs-plus@30/bin/emacs-30.2" ]; then
    EMACS_CMD="/opt/homebrew/opt/emacs-plus@30/bin/emacs-30.2"
elif command -v emacs > /dev/null 2>&1; then
    EMACS_CMD="emacs"
fi
echo "  Emacs: $EMACS_CMD ($($EMACS_CMD --version 2>&1 | head -1))"

cd "$HYDRA_EL_DIR"
$EMACS_CMD --batch --no-init-file \
     --eval "(load (expand-file-name \"src/main/emacs-lisp/hydra/bootstrap.el\") nil t)" \
     -- $EXTRA_ARGS
