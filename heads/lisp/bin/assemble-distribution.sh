#!/usr/bin/env bash
# Layer 2 dispatcher: delegate Lisp assembly to the appropriate per-dialect
# assembler.
#
# Usage:
#   assemble-distribution.sh <pkg> <dialect> [--dist-root <dir>]
#
# Dialects: clojure, scheme, common-lisp, emacs-lisp.
#
# Per-dialect assemblers live at heads/lisp/<dialect>/bin/assemble-distribution.sh.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <package> <dialect> [--dist-root <dir>]" >&2
    echo "Dialects: clojure, scheme, common-lisp, emacs-lisp" >&2
    exit 1
fi

PACKAGE="$1"
DIALECT="$2"
shift 2

case "$DIALECT" in
    clojure|scheme|common-lisp|emacs-lisp) ;;
    *)
        echo "Error: unknown Lisp dialect: $DIALECT" >&2
        exit 1
        ;;
esac

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_LISP_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"

exec "$HYDRA_LISP_HEAD/$DIALECT/bin/assemble-distribution.sh" "$PACKAGE" "$@"
