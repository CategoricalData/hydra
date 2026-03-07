#!/bin/bash
# Build and test a Haskell bootstrap target directory.
#
# Usage: ./test-haskell-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Building Haskell target..."
cd "$OUTPUT_DIR"
stack build 2>&1

echo "Running Haskell tests..."
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" stack test 2>&1
