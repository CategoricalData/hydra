#!/usr/bin/env bash
# Regenerate the Go-only sync matrix: --hosts go --targets go.
# Thin wrapper around sync.sh; extra flags (e.g. --no-tests) are forwarded.
#
# Note: Go is a "head bud" — Phase 4 host=go rows are skipped because
# the Go runtime cannot yet host generation of coder packages in Go's
# own language. See bin/sync.sh and issue #289.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" --hosts go --targets go "$@"
