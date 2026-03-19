#!/bin/bash
# Build and test a Scheme bootstrap target directory.
#
# Usage: ./test-scheme-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Running Scheme tests..."
cd "$OUTPUT_DIR"

# Detect Scheme implementation
if command -v guile > /dev/null 2>&1; then
    SCHEME_CMD="guile -L src/gen-main/scheme -L src/gen-test/scheme -L src/main/scheme -s run-tests.scm"
elif command -v chibi-scheme > /dev/null 2>&1; then
    SCHEME_CMD="chibi-scheme -I src/gen-main/scheme -I src/gen-test/scheme -I src/main/scheme run-tests.scm"
else
    echo "Error: No Scheme implementation found. Install guile or chibi-scheme."
    exit 1
fi

HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" $SCHEME_CMD 2>&1
