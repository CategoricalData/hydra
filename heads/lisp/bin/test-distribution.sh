#!/usr/bin/env bash
# Layer 2.5 dispatcher: delegate Lisp testing to the appropriate per-dialect
# tester.
#
# Usage:
#   test-distribution.sh <pkg> <dialect>

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <package> <dialect>" >&2
    exit 1
fi

PACKAGE="$1"
DIALECT="$2"

case "$DIALECT" in
    clojure|scheme|common-lisp|emacs-lisp) ;;
    *)
        echo "Error: unknown Lisp dialect: $DIALECT" >&2
        exit 1
        ;;
esac

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_LISP_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"

exec "$HYDRA_LISP_HEAD/$DIALECT/bin/test-distribution.sh" "$PACKAGE"
