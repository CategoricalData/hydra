#!/usr/bin/env bash
set -euo pipefail

# Run the Hydra test suite for a Lisp dialect.
#
# Usage: ./run-tests.sh <dialect>
#   dialect ∈ {clojure, common-lisp, emacs-lisp, scheme}
#
# Environment:
#   HYDRA_BENCHMARK_OUTPUT  If set, writes benchmark JSON to this path
#                            (honored by each dialect's own test runner)

if [ $# -lt 1 ]; then
    echo "Usage: $0 <clojure|common-lisp|emacs-lisp|scheme>" >&2
    exit 2
fi

DIALECT="$1"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LISP_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$LISP_ROOT/../.." && pwd )"
HEADS_LISP="$HYDRA_ROOT/heads/lisp"
PKG_LISP="$LISP_ROOT"

# HEAD_DIR has hand-written content (src/main, src/test, runner scripts, deps.edn etc.)
# Generated content is in dist/<dialect>/hydra-kernel/src/{main,test}
# Export HYDRA_HEAD_DIR so dialect-specific runners can find hand-written sources.
dialect_pkg_name() {
    case "$1" in
        clojure)     echo "hydra-clojure" ;;
        common-lisp) echo "hydra-common-lisp" ;;
        emacs-lisp)  echo "hydra-emacs-lisp" ;;
        scheme)      echo "hydra-scheme" ;;
    esac
}

HEAD_DIR="$HEADS_LISP/$DIALECT"
PKG_DIR="$PKG_LISP/$(dialect_pkg_name "$DIALECT")"
export HYDRA_HEAD_DIR="$HEAD_DIR"

case "$DIALECT" in
    clojure)
        cd "$HEAD_DIR"
        clojure -M -m run-tests 2>&1
        ;;
    common-lisp)
        cd "$PKG_DIR"
        sbcl --noinform --non-interactive --no-userinit \
             --load "$HEAD_DIR/src/test/common-lisp/run-tests.lisp" 2>&1 \
          | grep -v "STYLE-WARNING\|caught.*WARNING"
        ;;
    emacs-lisp)
        cd "$PKG_DIR"
        emacs --batch --load "$HEAD_DIR/run-tests.el" 2>&1 \
          | grep -v "^run-tests.el: Warning:"
        ;;
    scheme)
        cd "$HEAD_DIR"
        GEN_MAIN="$HYDRA_ROOT/dist/scheme/hydra-kernel/src/main/scheme"
        GEN_TEST="$HYDRA_ROOT/dist/scheme/hydra-kernel/src/test/scheme"
        if command -v guile > /dev/null 2>&1; then
            guile --no-auto-compile -L "$GEN_MAIN" -L "$GEN_TEST" -L src/main/scheme -s run-tests.scm 2>/dev/null
        elif command -v chibi-scheme > /dev/null 2>&1; then
            chibi-scheme -I "$GEN_MAIN" -I "$GEN_TEST" -I src/main/scheme run-tests.scm 2>/dev/null
        else
            echo "Error: No Scheme implementation found. Install guile or chibi-scheme." >&2
            exit 1
        fi
        ;;
    *)
        echo "Error: Unknown dialect '$DIALECT'. Valid: clojure, common-lisp, emacs-lisp, scheme" >&2
        exit 2
        ;;
esac
