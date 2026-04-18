#!/usr/bin/env bash
# Run the Wasm head's test suite under the default runtime (Node).
#
# M1: single hand-written manifest at heads/wasm/m1-manifest.json.
# Later milestones will discover manifests automatically.
#
# Usage:
#   heads/wasm/bin/test-wasm.sh [manifest.json]
#
# If no manifest is given, defaults to heads/wasm/m1-manifest.json.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WASM_HEAD_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
HYDRA_ROOT_DIR="$(cd "$WASM_HEAD_DIR/../.." && pwd)"

MANIFEST="${1:-$WASM_HEAD_DIR/m1-manifest.json}"
HARNESS="$WASM_HEAD_DIR/runtimes/node/harness.js"

# Resolve wasm_file relative to the worktree root.
WAT_REL=$(python3 -c "import json,sys; print(json.load(open('$MANIFEST'))['wasm_file'])")
WAT_PATH="$HYDRA_ROOT_DIR/$WAT_REL"

if [ ! -f "$WAT_PATH" ]; then
    echo "Error: WAT file not found: $WAT_PATH" >&2
    echo "       Regenerate with: heads/haskell/bin/sync-wasm.sh" >&2
    exit 1
fi

if ! command -v node >/dev/null 2>&1; then
    echo "Error: node not found. Install Node.js >= 20." >&2
    exit 1
fi

if ! command -v wat2wasm >/dev/null 2>&1; then
    echo "Error: wat2wasm not found. Install wabt: brew install wabt" >&2
    exit 1
fi

echo "Running Wasm tests: $MANIFEST"
echo "  WAT:      $WAT_REL"
echo "  Runtime:  node"
echo ""

exec node "$HARNESS" "$WAT_PATH" "$MANIFEST"
