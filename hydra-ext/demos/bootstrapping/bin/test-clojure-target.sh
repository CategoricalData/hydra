#!/bin/bash
# Build and test a Clojure bootstrap target directory.
#
# Usage: ./test-clojure-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Running Clojure tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" clojure -M -m run-tests 2>&1
