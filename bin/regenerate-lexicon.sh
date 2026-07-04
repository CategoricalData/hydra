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
#
# Failure handling: this script must FAIL LOUDLY when the ghci run fails
# or the lexicon is not actually rewritten. An earlier version piped the
# ghci output through `grep ... || true`, which masked every failure mode:
# a broken generator left the old lexicon file in place and exited 0, so
# prepare-release.sh's freshness gate (regenerate + diff) vacuously passed.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_ROOT/heads/haskell"

LOG_FILE="$(mktemp)"
trap 'rm -f "$LOG_FILE"' EXIT

echo "Regenerating docs/hydra-lexicon.txt..."
if ! stack ghci hydra:lib \
    --ghci-options='-e ":m Hydra.Haskell.Generation" -e "writeLexiconToStandardPath"' \
    > "$LOG_FILE" 2>&1; then
    echo "ERROR: stack ghci exited non-zero. Last 30 lines of output:" >&2
    tail -30 "$LOG_FILE" >&2
    exit 1
fi

# Positive confirmation: writeLexiconToStandardPath prints "Lexicon written to <path>"
# on success. A ghci session that loaded but failed to evaluate (e.g. after a module
# rename) can still exit 0, so check for the success line explicitly.
if ! grep -q "^Lexicon written to" "$LOG_FILE"; then
    echo "ERROR: lexicon was not written (no 'Lexicon written to' line in output)." >&2
    echo "Last 30 lines of output:" >&2
    tail -30 "$LOG_FILE" >&2
    exit 1
fi

grep -E "^Lexicon" "$LOG_FILE"
echo "Done."
