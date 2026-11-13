#!/usr/bin/env bash
# Wrapper script to generate kernel modules to JSON

# --- #814 build-slot guard: this script runs `stack build`/`exec`/`test` directly, so serialize it under the
# shared-~/.stack slot unless already held (reentrant: sync.sh and the other guarded drivers call it). Placed
# before any argument parsing so the ORIGINAL argv is forwarded; help-only invocations skip the slot. ---
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "--help" ] && [ "${1:-}" != "-h" ]; then
  exec "$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )/bin/with-stack-slot.sh" --label "update-json-kernel.sh" -- "$0" "$@"
fi

set -e

cd "$(dirname "$0")/.."

echo "Building update-json-kernel executable..."
stack build hydra:update-json-kernel

echo ""
echo "Running update-json-kernel..."
stack exec update-json-kernel -- --output-dir "$(pwd)/../../dist/json/hydra-kernel/src/main/json"
