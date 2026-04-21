#!/usr/bin/env bash
# Regenerate the Emacs Lisp-only sync matrix:
#   --hosts emacs-lisp --targets emacs-lisp.
# Thin wrapper around sync.sh; extra flags (e.g. --no-tests) are forwarded.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" --hosts emacs-lisp --targets emacs-lisp "$@"
