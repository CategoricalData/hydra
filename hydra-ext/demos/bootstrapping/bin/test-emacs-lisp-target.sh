#!/bin/bash
# Build and test an Emacs Lisp bootstrap target directory.
#
# Usage: ./test-emacs-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

echo "Running Emacs Lisp tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" emacs --batch --load run-tests.el 2>&1
