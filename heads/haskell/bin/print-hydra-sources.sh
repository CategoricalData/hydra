#!/usr/bin/env bash
# Print textually-sourced kernel modules from their Haskell DSL to their .hydra path (#837/#842).
#
# This is the SUPPORTED tool for the dual-edit rule: whenever a kernel module's Haskell DSL
# source changes, run this to regenerate the matching .hydra file(s) in the same commit. It is
# the print-side counterpart of bin/verify-hydra-sources.sh (which checks the .hydra -> JSON
# direction); this one regenerates .hydra -> text from the DSL and round-trip-checks it before
# writing, so a printer/parser mismatch is reported, not silently written.
#
# Usage:
#   bin/print-hydra-sources.sh                      -- every module declared textually sourced
#                                                        (every package.json's "textualSources"
#                                                        field, honoring "textualSourcesExclude"; since
#                                                        0.18.1 no kernel module is excluded)
#   bin/print-hydra-sources.sh <ns1> <ns2> ...      -- only the named modules, each of which must
#                                                        already be in the textual-sources scope
# Exits non-zero if any selected module fails to round-trip, or a named module is out of scope.
#
# Does not yet respect the comment safe-write rule (#842 card 83): once hand-added '#' comments
# exist in a .hydra file, overwriting it here would silently drop them. That guard is planned but
# not yet implemented -- for now, do not run this against a file you know carries hand-added
# comments.

# --- #814 build-slot guard: this script runs `stack build`/`exec` directly, so serialize it under the
# shared-~/.stack slot unless already held (reentrant: sync.sh and the other guarded drivers call it). Placed
# before any argument parsing so the ORIGINAL argv is forwarded; help-only invocations skip the slot. ---
if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ] && [ "${1:-}" != "--help" ] && [ "${1:-}" != "-h" ]; then
  exec "$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )/bin/with-stack-slot.sh" --label "print-hydra-sources.sh" -- "$0" "$@"
fi

set -e

cd "$(dirname "$0")/.."

echo "Building print-hydra-sources executable..."
stack build hydra:print-hydra-sources

echo ""
echo "Running print-hydra-sources..."
stack exec print-hydra-sources -- "$@"
