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
i=0
while [ $i -lt $# ]; do
    arg="${!i}"; i=$((i + 1))
    # Bash arrays are 0-based but $@ is 1-based; shift through args instead
    :
done
# Re-parse using positional params
TARGET=""
OUTPUT_BASE=""
PASSTHROUGH_ARGS=()
skip_next=false
for ((i=1; i<=$#; i++)); do
    if $skip_next; then
        skip_next=false
        continue
    fi
    arg="${!i}"
    next_i=$((i + 1))
    case "$arg" in
        --target)
            TARGET="${!next_i}"
            skip_next=true
            ;;
        --output)
            OUTPUT_BASE="${!next_i}"
            skip_next=true
            ;;
        *)
            PASSTHROUGH_ARGS+=("$arg")
            ;;
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
