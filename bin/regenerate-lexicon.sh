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

# Capture the full ghci output so we can both inspect it for the success marker
# and surface it verbatim on failure. We must NOT pipe `stack ghci` straight into
# `grep`: that would make the pipeline's exit status grep's, hiding a ghci failure,
# and the narrow pattern would discard the actual error line. `writeLexiconToStandardPath`
# can also fail *inside* ghci (e.g. "Lexicon generation failed: ...") while ghci itself
# exits 0, so we additionally require the "Lexicon written to" marker.
log="$(mktemp -t regenerate-lexicon.XXXXXX)"
trap 'rm -f "$log"' EXIT

set +e
stack ghci hydra:lib \
    --ghci-options='-e ":m Hydra.Haskell.Generation" -e "writeLexiconToStandardPath"' \
    >"$log" 2>&1
status=$?
set -e

# Show the salient lines (success marker, errors) without burying them in compiler warnings.
grep -E "^Lexicon|Lexicon written|Lexicon generation failed|^Error|does not exist|no such binding|user error" "$log" || true

if [ "$status" -ne 0 ] || ! grep -q "Lexicon written to" "$log"; then
    echo "ERROR: lexicon regeneration failed (ghci exit $status; no 'Lexicon written' marker)." >&2
    echo "Full output:" >&2
    cat "$log" >&2
    exit 1
fi

echo "Done."
