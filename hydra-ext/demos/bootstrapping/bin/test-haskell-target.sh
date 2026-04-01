#!/bin/bash
# Build and test a Haskell bootstrap target directory.
#
# Usage: ./test-haskell-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# Patch TestGraph.hs to use TestEnv (real graph with primitives) instead of emptyGraph.
# TODO: Replace this with hydra.test.environment module.
echo "Patching TestGraph.hs..."
TESTGRAPH="$OUTPUT_DIR/src/gen-test/haskell/Hydra/Test/TestGraph.hs"
if [ -f "$TESTGRAPH" ]; then
    sed -i '' 's/import qualified Hydra.Lexical as Lexical$/import qualified Hydra.Lexical as Lexical\nimport qualified Hydra.Test.TestEnv as TestEnv/' "$TESTGRAPH"
    sed -i '' 's/testGraph = Lexical.emptyGraph/testGraph = TestEnv.testGraph testTypes/' "$TESTGRAPH"
    sed -i '' 's/testContext = Lexical.emptyContext/testContext = TestEnv.testContext/' "$TESTGRAPH"
fi

echo "Building Haskell target..."
cd "$OUTPUT_DIR"
stack build 2>&1

echo "Running Haskell tests..."
if [ -n "${HYDRA_BENCHMARK_OUTPUT:-}" ]; then
    HYDRA_BENCHMARK_OUTPUT="$HYDRA_BENCHMARK_OUTPUT" stack test 2>&1
else
    stack test 2>&1
fi
