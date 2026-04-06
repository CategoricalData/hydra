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

case "$DIALECT" in
    clojure)
        cd "$LISP_ROOT/hydra-clojure"
        clojure -M -m run-tests 2>&1
        ;;
    common-lisp)
        cd "$LISP_ROOT/hydra-common-lisp"
        sbcl --noinform --non-interactive --no-userinit \
             --load src/test/common-lisp/run-tests.lisp 2>&1 \
          | grep -v "STYLE-WARNING\|caught.*WARNING"
        ;;
    emacs-lisp)
        cd "$LISP_ROOT/hydra-emacs-lisp"
        emacs --batch --load run-tests.el 2>&1 \
          | grep -v "^run-tests.el: Warning:"
        ;;
    scheme)
        cd "$LISP_ROOT/hydra-scheme"
        if command -v guile > /dev/null 2>&1; then
            guile --no-auto-compile -L src/gen-main/scheme -L src/gen-test/scheme -L src/main/scheme -s run-tests.scm 2>/dev/null
        elif command -v chibi-scheme > /dev/null 2>&1; then
            chibi-scheme -I src/gen-main/scheme -I src/gen-test/scheme -I src/main/scheme run-tests.scm 2>/dev/null
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
