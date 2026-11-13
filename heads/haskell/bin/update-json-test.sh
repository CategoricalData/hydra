#!/usr/bin/env bash
# Wrapper script to generate test modules to JSON
#
# NOT DEAD CODE: no automated callers by design; documented manual entry point
# (docs/recipes/json-kernel.md § "Export test modules to JSON"), the test-module
# companion to update-json-main.sh. See docs/build-system.md § "Build & sync scripts". (#714)

# --- #814 build-slot guard: this script runs `stack build`/`exec`/`test` directly, so serialize it under the
# shared-~/.stack slot unless already held (reentrant: sync.sh and the other guarded drivers call it). Placed
# before any argument parsing so the ORIGINAL argv is forwarded; help-only invocations skip the slot. ---
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "--help" ] && [ "${1:-}" != "-h" ]; then
  exec "$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )/bin/with-stack-slot.sh" --label "update-json-test.sh" -- "$0" "$@"
fi

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-test executable..."
stack build hydra:update-json-test

echo ""
echo "Running update-json-test..."
stack exec update-json-test -- --output-dir "$(pwd)/../../dist/json/hydra-kernel/src/test/json"
