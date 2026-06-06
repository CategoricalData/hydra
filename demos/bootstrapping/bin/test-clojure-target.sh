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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_CLOJURE_DIR="$HYDRA_ROOT/heads/lisp/clojure"
CLOJURE_RESOURCES="$SCRIPT_DIR/../resources/clojure"

# Defensive re-copy of static resources that setup-clojure-target.sh laid down.
# In the typescript-host cells (#444), these were observed missing at test time,
# even though the setup script ran. Root cause is unidentified; this guarantees
# the test step has the runner regardless. Idempotent: if setup already populated
# the dir, these cp's are no-ops on content. Mirrors setup-clojure-target.sh.
if [ ! -f "$OUTPUT_DIR/deps.edn" ]; then
    cp "$CLOJURE_RESOURCES/deps.edn" "$OUTPUT_DIR/"
fi
if [ ! -f "$OUTPUT_DIR/src/test/clojure/run_tests.clj" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/clojure"
    cp "$HYDRA_CLOJURE_DIR/src/test/clojure/run_tests.clj" "$OUTPUT_DIR/src/test/clojure/"
fi
# testGraph.clj post-generation patch removed. The DSL emits
# (hydra.test.testEnv :refer :all) and references
# hydra_test_test_env_test_{context,graph} directly. Copy the
# hand-written testEnv.clj (plus sibling test_runner.clj / annotation_bindings.clj)
# into the dist tree so the generated requires resolve.
if [ -d "$HYDRA_CLOJURE_DIR/src/test/clojure/hydra" ]; then
    mkdir -p "$OUTPUT_DIR/src/test/clojure/hydra"
    cp -r "$HYDRA_CLOJURE_DIR/src/test/clojure/hydra/." "$OUTPUT_DIR/src/test/clojure/hydra/"
fi

echo "Running Clojure tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" clojure -M -m run-tests 2>&1
