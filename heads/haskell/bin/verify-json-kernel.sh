#!/usr/bin/env bash
# Wrapper script to verify kernel modules JSON export consistency
#
# NOT DEAD CODE: no automated callers by design. The documented manual "verify JSON
# round-trips" tool, referenced by 8 docs. See docs/build-system.md § "Build & sync
# scripts". (#714)

# --- #814 build-slot guard: this script runs `stack build`/`exec`/`test` directly, so serialize it under the
# shared-~/.stack slot unless already held (reentrant: sync.sh and the other guarded drivers call it). Placed
# before any argument parsing so the ORIGINAL argv is forwarded; help-only invocations skip the slot. ---
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "--help" ] && [ "${1:-}" != "-h" ]; then
  exec "$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )/bin/with-stack-slot.sh" --label "verify-json-kernel.sh" -- "$0" "$@"
fi

set -e

cd "$(dirname "$0")/.."

echo "Building verify-json-kernel executable..."
stack build hydra:verify-json-kernel

echo ""
echo "Running verify-json-kernel..."
stack exec verify-json-kernel
