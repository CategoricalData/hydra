#!/usr/bin/env bash
# Layer 1 transform: JSON -> Python, scoped to a single package.
# Thin wrapper over transform-json-to-target.sh. See that script for details.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
exec "$SCRIPT_DIR/transform-json-to-target.sh" python "$@"
