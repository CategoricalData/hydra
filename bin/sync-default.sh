#!/usr/bin/env bash
# Zero-arg convenience wrapper for the default Java x Python matrix.
#
# Equivalent to:
#   bin/sync.sh --hosts java,python --targets java,python
#
# Haskell is opt-in (pass --hosts/--targets including haskell explicitly, or
# use bin/sync.sh directly) -- reserved for changes that specifically concern
# Haskell-authoring source, per the fleet-wide policy of not blocking ordinary
# per-worktree sync/test cycles on a Haskell build by default.
#
# Extra flags (e.g. --no-tests) are forwarded to sync.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" \
    --hosts java,python \
    --targets java,python \
    "$@"
