#!/bin/bash
# Build and test a Haskell bootstrap target directory.
#
# Usage: ./test-haskell-target.sh <output-dir>

# --- #814 build-slot guard: this script runs `stack build`/`exec`/`test` directly, so serialize it under the
# shared-~/.stack slot unless already held (reentrant: sync.sh and the other guarded drivers call it). Placed
# before any argument parsing so the ORIGINAL argv is forwarded; help-only invocations skip the slot. ---
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "--help" ] && [ "${1:-}" != "-h" ]; then
  exec "$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )/bin/with-stack-slot.sh" --label "test-haskell-target.sh" -- "$0" "$@"
fi

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# TestGraph.hs Lexical.empty post-generation patch eliminated: the DSL
# now emits testGraph = TestEnv.testGraph testTypes testTerms and
# testContext = TestEnv.testContext directly. Sed patterns matched
# nothing in regenerated output.

# NaN/Inf literal post-generation patch eliminated: the Java and Python
# Haskell coders' literalToExpr already emits (0/0), (1/0), (-(1/0)) for
# Float NaN/Infinity literals via the showFloat helper inherited from
# the DSL source (packages/hydra-haskell/.../Sources/Haskell/Serde.hs).
# The previous sed patterns ' NaN)' / ' Infinity)' matched zero
# occurrences in regenerated output — only string-literal occurrences
# (test case names like "sin NaN") remain, which were never patched.

echo "Building Haskell target..."
cd "$OUTPUT_DIR"
stack build 2>&1

echo "Running Haskell tests..."
if [ -n "${HYDRA_BENCHMARK_OUTPUT:-}" ]; then
    HYDRA_BENCHMARK_OUTPUT="$HYDRA_BENCHMARK_OUTPUT" stack test 2>&1
else
    stack test 2>&1
fi
