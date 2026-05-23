#!/usr/bin/env bash
# Regenerate the TypeScript-only sync matrix: --hosts typescript --targets typescript.
# Thin wrapper around sync.sh; extra flags (e.g. --no-tests) are forwarded.
#
# Note: TypeScript is a "head bud" — Phase 4 host=typescript rows are
# skipped because the TypeScript runtime cannot yet host generation of
# any coder package in TypeScript's own language. See bin/sync.sh and
# issue #126.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/sync.sh" --hosts typescript --targets typescript "$@"
