#!/usr/bin/env bash
# Regenerate the Common Lisp-only sync matrix:
#   --hosts common-lisp --targets common-lisp.
# Thin wrapper around sync.sh; extra flags (e.g. --no-tests) are forwarded.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" --hosts common-lisp --targets common-lisp "$@"
