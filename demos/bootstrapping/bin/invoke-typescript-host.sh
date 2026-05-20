#!/bin/bash
# TypeScript bootstrapping demo: loads Hydra modules from JSON and
# generates code for a target language via the TS-implemented coders in
# dist/typescript/hydra-<target>/.
#
# Usage: ./invoke-typescript-host.sh --target <typescript|python|java|haskell|scala> [--include-tests] [--kernel-only] [--types-only] [--output <dir>]
#
# When --output is provided, output goes to <dir>/typescript-to-<target>/.

set -eo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"

TARGET=""
OUTPUT_BASE=""
PASSTHROUGH_ARGS=()
while [ $# -gt 0 ]; do
    case "$1" in
        --target) TARGET="$2"; shift 2 ;;
        --target=*) TARGET="${1#--target=}"; shift ;;
        --output) OUTPUT_BASE="$2"; shift 2 ;;
        --output=*) OUTPUT_BASE="${1#--output=}"; shift ;;
        *) PASSTHROUGH_ARGS+=("$1"); shift ;;
    esac
done

if [ -z "$TARGET" ]; then
    echo "ERROR: --target is required"
    echo "Usage: $0 --target <lang> [--output <dir>] [--include-tests] [--kernel-only] [--types-only]"
    exit 1
fi

OUTPUT_ARGS=()
if [ -n "$OUTPUT_BASE" ]; then
    OUTPUT_DIR="$OUTPUT_BASE/typescript-to-$TARGET"
    OUTPUT_ARGS=(--output "$OUTPUT_DIR")
fi

# The TS bootstrap driver lives in the dist tree alongside the kernel runtime.
# It needs Node 20+ with tsx for ESM .ts execution.
BOOTSTRAP_TS="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra/bootstrap.ts"
if [ ! -f "$BOOTSTRAP_TS" ]; then
    echo "ERROR: $BOOTSTRAP_TS not found. Run heads/typescript/bin/copy-kernel-runtime.sh first."
    exit 1
fi

cd "$HYDRA_ROOT/heads/typescript"
# tsx is in heads/typescript/node_modules; use it via npx.
echo "TypeScript host: generating $TARGET code..."
echo ""
npx --yes tsx "$BOOTSTRAP_TS" \
    --target "$TARGET" \
    --json-dir "$HYDRA_ROOT/dist/json/hydra-kernel/src/main/json" \
    "${OUTPUT_ARGS[@]}" \
    "${PASSTHROUGH_ARGS[@]}"
