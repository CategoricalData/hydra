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

# test_graph.lisp post-generation patch removed. The DSL emits
# hydra_test_test_env_test_{context,graph} refs directly, and the
# hand-written heads/lisp/common-lisp/.../test_env.lisp resolves them.

echo "Running Common Lisp tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" sbcl --noinform --non-interactive --no-userinit --load src/test/common-lisp/run-tests.lisp 2>&1
