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

# testGraph.clj post-generation patch removed. The DSL emits
# (hydra.test.testEnv :refer :all) and references
# hydra_test_test_env_test_{context,graph} directly. Copy the
# hand-written testEnv.clj into the dist tree so the generated require
# resolves.
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
TEST_ENV_SRC="$HYDRA_ROOT/heads/lisp/clojure/src/test/clojure/hydra/test/testEnv.clj"
TEST_ENV_DST="$OUTPUT_DIR/src/test/clojure/hydra/test/testEnv.clj"
if [ -f "$TEST_ENV_SRC" ]; then
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

echo "Running Clojure tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" clojure -M -m run-tests 2>&1
