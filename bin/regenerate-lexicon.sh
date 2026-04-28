#!/usr/bin/env bash
set -euo pipefail

# Regenerate docs/hydra-lexicon.txt from the current Haskell kernel.
#
# The lexicon is a human-readable summary of every kernel type and
# primitive, intended as an LLM reference. It is regenerated on demand
# (and as part of the pre-release verification flow) rather than on
# every sync, because regeneration takes ~4 minutes and the lexicon
# does not feed any downstream build step.
#
# Usage:
#   ./bin/regenerate-lexicon.sh
#
# Prerequisites: Stack, with the hydra library buildable.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_ROOT/heads/haskell"

echo "Regenerating docs/hydra-lexicon.txt..."
stack ghci hydra:lib \
    --ghci-options='-e ":m Hydra.Haskell.Generation" -e "writeLexiconToStandardPath"' \
    2>&1 | grep -E "^Lexicon|^Error|does not exist" || true

echo "Done."
