#!/usr/bin/env bash
# Run the standalone regression harnesses that guard specific prune/reconcile
# bugs. Each script was written to pin down a bug that had already bitten
# once; unrun, they rot silently and the invariants they guard can regress
# unnoticed. This script gives them a single entry point for local use
# (bin/test.sh --regressions) and CI.
#
# Resolves #535.
#
# Scripts run, in order:
#   bin/test-orphan-reconcile.sh              (guards #393, hermetic)
#   bin/test-json-orphan-reconcile.sh         (guards #405, needs synced dist/json; SKIPs cleanly otherwise)
#   bin/test-json-content-invalidates-render.sh (guards #469, needs synced dist/json; SKIPs cleanly otherwise)
#   bin/test-stale-output-prune.sh            (guards #357, hermetic)
#   bin/test-test-digest-freshness.sh         (guards #551, needs synced dist/json; SKIPs cleanly otherwise)
#
# All five build their own Haskell executables from current source before
# exec'ing them, so this script does not require a pre-built stack project —
# only a Haskell toolchain. Run after a sync (so dist/json/ is populated) to
# exercise the #405, #469, and #551 cases; without a sync those three SKIP
# rather than fail.
#
# Usage:
#   bin/test-regressions.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SCRIPTS=(
    "test-orphan-reconcile.sh"
    "test-json-orphan-reconcile.sh"
    "test-json-content-invalidates-render.sh"
    "test-stale-output-prune.sh"
    "test-test-digest-freshness.sh"
)

declare -a RESULTS=()
declare -a FAILED=()
OVERALL_RC=0

for s in "${SCRIPTS[@]}"; do
    echo "=== $s ==="
    if "$SCRIPT_DIR/$s"; then
        RESULTS+=("$s: PASS")
    else
        RESULTS+=("$s: FAIL")
        FAILED+=("$s")
        OVERALL_RC=1
    fi
    echo ""
done

echo "==========================================="
echo "  Regression harness summary"
echo "==========================================="
for r in "${RESULTS[@]}"; do
    echo "  $r"
done
echo ""

if [ "$OVERALL_RC" = "0" ]; then
    echo "All regression harnesses passed."
else
    echo "Failed: ${FAILED[*]}"
fi

exit "$OVERALL_RC"
