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
#   bin/test-haskell-bootstrap-demo-deps-sync.sh (guards #670, hermetic)
#   bin/test-check-oil-and-water.sh           (guards #608, hermetic — synthetic fixtures + python3)
#   bin/test-header-idempotency.sh            (guards #540, hermetic — restores heads/haskell/*.yaml on exit)
#   bin/test-assembly-plan-conformance.sh     (guards #416, hermetic — apply-assembly-plan.sh ≡ plan oracle)
#
# The first five build their own Haskell executables from current source
# before exec'ing them, so this script does not require a pre-built stack
# project — only a Haskell toolchain. Run after a sync (so dist/json/ is
# populated) to exercise the #405, #469, and #551 cases; without a sync those
# three SKIP rather than fail. #670's script is pure Python and needs neither.
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
    "test-haskell-bootstrap-demo-deps-sync.sh"
    "test-check-oil-and-water.sh"
    "test-header-idempotency.sh"
    "test-assembly-plan-conformance.sh"
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
