#!/usr/bin/env bash
# Layer 1 transform: JSON -> a Lisp dialect, scoped to a single package.
#
# Usage:
#   transform-json-to-lisp.sh <pkg> <dialect> [main|test] [OPTIONS]
#
# Dialect must be one of: clojure, scheme, common-lisp, emacs-lisp.
#
# Thin wrapper over transform-json-to-target.sh, mapping the dialect name
# to the corresponding --target value accepted by bootstrap-from-json.
#
# This is a Layer 1 transform per feature_290_packaging-plan.md.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <package> <dialect> [main|test] [OPTIONS]" >&2
    echo "" >&2
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
        echo "Valid dialects: clojure, scheme, common-lisp, emacs-lisp" >&2
        exit 1
        ;;
esac

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/transform-json-to-target.sh" "$DIALECT" "$PACKAGE" "$@"
