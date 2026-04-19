#!/usr/bin/env bash
# Regenerate the Python-only sync matrix: --hosts python --targets python.
# Thin wrapper around sync.sh; extra flags (e.g. --no-tests) are forwarded.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" --hosts python --targets python "$@"
