#!/bin/bash
# Build and test a Common Lisp bootstrap target directory.
#
# Usage: ./test-common-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Running Common Lisp tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" sbcl --noinform --non-interactive --no-userinit --load src/test/common-lisp/run-tests.lisp 2>&1
