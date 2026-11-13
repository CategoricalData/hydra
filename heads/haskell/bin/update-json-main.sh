#!/usr/bin/env bash
# Wrapper script to generate every package's main modules to JSON.
# Writes fanned-out output to dist/json/<package>/src/main/json/ based on
# the routing table in Hydra.PackageRouting.
#
# NOT DEAD CODE: no automated callers by design. Documented manual entry point
# (docs/recipes/json-kernel.md § "Export all packages to JSON") for regenerating
# dist/json by hand — distinct from /sync, which calls the update-json-main
# *executable* directly. See docs/build-system.md § "Build & sync scripts". (#714)

# --- #814 build-slot guard: this script runs `stack build`/`exec`/`test` directly, so serialize it under the
# shared-~/.stack slot unless already held (reentrant: sync.sh and the other guarded drivers call it). Placed
# before any argument parsing so the ORIGINAL argv is forwarded; help-only invocations skip the slot. ---
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "--help" ] && [ "${1:-}" != "-h" ]; then
  exec "$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )/bin/with-stack-slot.sh" --label "update-json-main.sh" -- "$0" "$@"
fi

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-main executable..."
stack build hydra:update-json-main

echo ""
echo "Running update-json-main..."
stack exec update-json-main -- --dist-root "$(pwd)/../../dist/json"
