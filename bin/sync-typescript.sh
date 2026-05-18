#!/usr/bin/env bash
# TypeScript sync.
#
# Three phases:
#   1. Regenerate hydra-typescript's own DSL sources into Haskell (via
#      bin/sync-packages.sh --targets haskell hydra-typescript). This is
#      what keeps the TypeScript coder + syntax model up to date in dist/.
#   2. Generate the hydra-kernel TypeScript output under
#      dist/typescript/hydra-kernel/src/main/typescript/hydra/.
#   3. Copy the hand-written TS runtime (heads/typescript/src/main/typescript/
#      hydra/{core.ts,primitives.ts,lib/*.ts}) into the same dist tree so
#      the generated code's `./lib/...` imports resolve.
#
# See issue #126. Once the TypeScript coder is fully integrated with the
# matrix tooling, this script can switch to bin/sync.sh.

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

# Phase 1: regenerate hydra-typescript Haskell sources.
"$SCRIPT_DIR/sync-packages.sh" --targets haskell hydra-typescript "$@"

# Phase 2: generate hydra-kernel into TypeScript (main + test sources).
echo ""
echo "=========================================="
echo "Generating hydra-kernel into TypeScript"
echo "=========================================="
"$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
    typescript hydra-kernel main \
    --output "$HYDRA_ROOT/dist/typescript"
"$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
    typescript hydra-kernel test \
    --output "$HYDRA_ROOT/dist/typescript"

# Phase 3: copy the hand-written runtime alongside the generated kernel.
echo ""
"$HYDRA_ROOT/heads/typescript/bin/copy-kernel-runtime.sh" \
    --dist-root "$HYDRA_ROOT/dist/typescript"

echo ""
echo "=========================================="
echo "TypeScript sync: DONE"
echo "=========================================="
