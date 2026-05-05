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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"

# test_graph.scm post-generation patch removed. The DSL emits
# (import (hydra test test_env)) and references hydra_test_test_env_test_*
# directly. Copy the hand-written test_env.scm into the dist tree so the
# generated import resolves (load path includes src/test/scheme).
TEST_ENV_SRC="$HYDRA_ROOT/heads/lisp/scheme/src/test/scheme/hydra/test/test_env.scm"
TEST_ENV_DST="$OUTPUT_DIR/src/test/scheme/hydra/test/test_env.scm"
if [ -f "$TEST_ENV_SRC" ]; then
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

echo "Running Scheme tests..."
cd "$OUTPUT_DIR"

# Detect Scheme implementation
if command -v guile > /dev/null 2>&1; then
    SCHEME_CMD="guile -L src/main/scheme -L src/test/scheme -L src/main/scheme -s run-tests.scm"
elif command -v chibi-scheme > /dev/null 2>&1; then
    SCHEME_CMD="chibi-scheme -I src/main/scheme -I src/test/scheme -I src/main/scheme run-tests.scm"
else
    echo "Error: No Scheme implementation found. Install guile or chibi-scheme."
    exit 1
fi

HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" $SCHEME_CMD 2>&1
