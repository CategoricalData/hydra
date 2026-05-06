#!/usr/bin/env bash
# Zero-arg convenience wrapper for the bootstrapping triad.
#
# Equivalent to:
#   bin/sync.sh --hosts haskell,java,python --targets haskell,java,python
#
# Extra flags (e.g. --no-tests) are forwarded to sync.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" \
    --hosts haskell,java,python \
    --targets haskell,java,python \
    "$@"
