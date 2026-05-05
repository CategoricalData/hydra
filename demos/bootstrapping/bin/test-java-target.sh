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

# TestGraph.java Lexical.empty post-generation patch eliminated: the DSL
# now emits TestEnv refs directly. Sed patterns matched nothing.

echo "Building Java target..."
cd "$OUTPUT_DIR"
./gradlew compileJava compileTestJava 2>&1

echo "Running Java tests..."
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" ./gradlew test 2>&1
