#!/bin/bash
# Build and test a Java bootstrap target directory.
#
# Usage: ./test-java-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# Patch TestGraph.java to use TestEnv (real graph with primitives) instead of emptyGraph.
# TODO: Replace this with hydra.test.environment module.
echo "Patching TestGraph.java..."
TESTGRAPH="$OUTPUT_DIR/src/gen-test/java/hydra/test/TestGraph.java"
if [ -f "$TESTGRAPH" ]; then
    sed -i '' 's/return hydra.Lexical.emptyGraph();/return hydra.TestEnv.testGraph();/' "$TESTGRAPH"
    sed -i '' 's/return hydra.Lexical.emptyContext();/return hydra.TestEnv.testContext();/' "$TESTGRAPH"
fi

echo "Building Java target..."
cd "$OUTPUT_DIR"
./gradlew compileJava compileTestJava 2>&1

echo "Running Java tests..."
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" ./gradlew test 2>&1
