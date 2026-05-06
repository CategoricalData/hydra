#!/bin/bash
# Haskell bootstrapping demo: loads Hydra modules from JSON and generates code.
# Demonstrates that Haskell can independently load and process Hydra modules
# from a language-independent JSON representation.
#
# Usage: ./invoke-haskell-host.sh --target <haskell|java|python> [--include-tests] [--kernel-only] [--types-only] [--output <dir>]
#
# When --output is provided, output goes to <dir>/haskell-to-<target>/.
#
# The detailed step-by-step output (timing, file counts, etc.) is provided
# by the bootstrap-from-json executable itself.

set -eo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EXT_DIR="$HYDRA_ROOT/heads/haskell"

# Parse --target and --output so we can compute the haskell-to-{target} subdirectory.
# All other flags are passed through unchanged.
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
    echo "Usage: $0 --target <haskell|java|python> [--output <dir>] [--include-tests] [--kernel-only] [--types-only]"
    exit 1
fi

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

# Build bootstrap-from-json
echo "Building bootstrap-from-json..."
BUILD_START=$(date +%s)
cd "$HYDRA_EXT_DIR"
stack build hydra:exe:bootstrap-from-json
BUILD_END=$(date +%s)
echo "  Build time: $((BUILD_END - BUILD_START))s"
echo ""

# Compute the actual output directory: {output_base}/haskell-to-{target}
# This matches the convention used by Java and Python bootstraps.
OUTPUT_ARGS=()
if [ -n "$OUTPUT_BASE" ]; then
    OUTPUT_DIR="$OUTPUT_BASE/haskell-to-$TARGET"
    OUTPUT_ARGS=(--output "$OUTPUT_DIR")
fi

# Run the Haskell bootstrap (its output includes detailed timing and file counts)
stack exec bootstrap-from-json -- --target "$TARGET" "${OUTPUT_ARGS[@]}" "${PASSTHROUGH_ARGS[@]}" --dist-json-root "$HYDRA_ROOT/dist/json" $RTS_FLAGS
