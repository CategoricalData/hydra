#!/usr/bin/env bash
# CI regression guard for inference cost (#700): time AND memory, not just a crash
# ceiling.
#
# bin/lib/common.sh's RTS_FLAGS already caps the Haskell generation execs at -M6G
# so a runaway inference pass fails loudly instead of being OOM-SIGKILL'd -- but
# that is a crash cliff, not a measurement: nothing records how CLOSE a given
# commit comes to it, and nothing fails on a time regression at all (#700's
# "unknown margin to the ceiling"). This test closes that gap by running the
# flat, cache-bypassing full-kernel inference path (the same worst-case shape as
# update-json-kernel's "one let over the whole universe", which update-json-main's
# normal per-package incremental route (#381) specifically avoids paying), reading
# GHC's own RTS stats (wall time + max_mem_in_use_bytes), and asserting both
# against the absolute budget recorded in
# docs/history/data/bench-records/inference-cost-guard-baseline.json.
#
# Workload: heads/haskell/src/exec/inference-cost-guard (stack exec
# inference-cost-guard), invoked with +RTS -M8G -T -RTS (this script passes
# them -- see INFERENCE_COST_GUARD_RTS_FLAGS below) so GHC.Stats.getRTSStats
# reports real numbers and a runaway pass is capped rather than threatening a
# shared machine. -with-rtsopts in package.yaml can't bake in a multi-flag
# RTS string (GHC splits "-M8G -T" on the space and rejects the bare -T as an
# unrecognised COMPILE-time flag), so these must be passed at invocation.
#
# KNOWN GAP: see the baseline file's own "IMPORTANT_memory_discrepancy" field.
# A real measurement (2026-10-11, module_count=185) recorded
# elapsed_seconds=193.13 (budget.max_elapsed_seconds=400 derived from this with
# ~2x headroom) but max_mem_in_use_bytes=7316963328 (~7.32GB), which EXCEEDS
# both budget.max_mem_in_use_bytes (left at the pre-existing 6GB, matching
# -M6G elsewhere) and the ~7GB GitHub Actions runner ceiling. The memory
# budget was deliberately NOT raised to match this measurement -- doing so
# would set an unachievable CI target. This measurement ran on a busy,
# concurrently-loaded fleet machine (not isolated), so some inflation from
# contention is possible, but flagging rather than silently resolving: this
# may be a real #700-relevant finding (margin-to-ceiling possibly already
# negative), not just a threshold-setting exercise. Needs a clean,
# isolated re-measurement + a human/coordinator decision before resolving.
#
# Oracle gating (matches test-digest-conformance.sh's convention): running this
# workload means `stack exec inference-cost-guard`, a real shared-~/.stack build/
# run that can collide with a concurrent sibling-worktree Haskell build. By
# default (test-regressions.sh, CI) the live run is OFF; the test only verifies
# the recorded baseline file is well-formed and SKIPs the live measurement. The
# live run executes ONLY when opted in with HYDRA_INFERENCE_COST_ORACLE=1, which
# the caller sets only while holding the Haskell build slot (staging-granted, or
# bin/with-stack-slot.sh wraps this whole script when invoked standalone).
#
# Usage:
#   bin/test-inference-cost-guard.sh                        # baseline-file sanity only (default)
#   HYDRA_INFERENCE_COST_ORACLE=1 bin/test-inference-cost-guard.sh   # live measurement + threshold assert
#   HYDRA_INFERENCE_COST_ORACLE=1 bin/test-inference-cost-guard.sh --record-baseline  # also update the baseline
#                                                                                     # file, but ONLY if the run
#                                                                                     # passes the existing budget
#                                                                                     # (a FAILing measurement is
#                                                                                     # never silently recorded)
#
# Wired into bin/test-regressions.sh. Exit 0 = within budget (or cleanly skipped
# live-run); nonzero = regression (time or memory over budget) or malformed
# baseline file.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"

# --- #730 build-slot guard: the live path runs `stack build`/`stack exec`.
# Self-re-exec under the shared slot unless already held, oracle is off, or
# this is just --help. ---
RECORD_BASELINE=0
for a in "$@"; do
    [ "$a" = "--record-baseline" ] && RECORD_BASELINE=1
done
if [ "${HYDRA_INFERENCE_COST_ORACLE:-0}" = "1" ] && [ -z "${HYDRA_STACK_SLOT_HELD:-}" ]; then
    exec "$HYDRA_ROOT_DIR/bin/with-stack-slot.sh" --label "test-inference-cost-guard.sh" -- "$0" "$@"
fi

BASELINE_FILE="$HYDRA_ROOT_DIR/docs/history/data/bench-records/inference-cost-guard-baseline.json"

fail=0
report_fail() { echo "FAIL: $*" >&2; fail=1; }
note() { echo "  $*"; }

echo "=== test-inference-cost-guard.sh (#700) ==="

if [ ! -f "$BASELINE_FILE" ]; then
    report_fail "baseline file missing: $BASELINE_FILE"
    exit 1
fi

if ! jq empty "$BASELINE_FILE" >/dev/null 2>&1; then
    report_fail "baseline file is not valid JSON: $BASELINE_FILE"
    exit 1
fi

MAX_SECONDS="$(jq -r '.budget.max_elapsed_seconds' "$BASELINE_FILE")"
MAX_MEM_BYTES="$(jq -r '.budget.max_mem_in_use_bytes' "$BASELINE_FILE")"

if [ "$MAX_SECONDS" = "null" ] || [ "$MAX_MEM_BYTES" = "null" ]; then
    report_fail "baseline file missing budget.max_elapsed_seconds or budget.max_mem_in_use_bytes"
    exit 1
fi
note "budget: max_elapsed_seconds=$MAX_SECONDS max_mem_in_use_bytes=$MAX_MEM_BYTES"

if [ "${HYDRA_INFERENCE_COST_ORACLE:-0}" != "1" ]; then
    note "SKIP: live measurement off (set HYDRA_INFERENCE_COST_ORACLE=1 with a stack slot to run it)."
    note "Baseline file is well-formed; exiting 0."
    exit 0
fi

TMP="$(mktemp -d -t hydra-inference-cost-guard.XXXXXX)"
trap 'rm -rf "$TMP"' EXIT
RESULT_JSON="$TMP/result.json"

echo ""
echo "=== Building inference-cost-guard ==="
( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack build hydra:exe:inference-cost-guard )

echo ""
echo "=== Running full-kernel inference (flat, cache-bypassing) ==="
# -M8G caps peak heap (safety net on a shared/loaded machine; mirrors but
# exceeds the existing -M6G RTS_FLAGS cap in bin/lib/common.sh, since this
# workload's real peak was still unknown as of the cap's introduction).
# -T is required for GHC.Stats.getRTSStats to report real numbers.
INFERENCE_COST_GUARD_RTS_FLAGS="+RTS -M8G -T -RTS"
( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack exec inference-cost-guard -- --out "$RESULT_JSON" $INFERENCE_COST_GUARD_RTS_FLAGS )

if ! jq empty "$RESULT_JSON" >/dev/null 2>&1; then
    report_fail "inference-cost-guard did not produce valid JSON at $RESULT_JSON"
    exit 1
fi

OK="$(jq -r '.ok' "$RESULT_JSON")"
ELAPSED="$(jq -r '.elapsed_seconds' "$RESULT_JSON")"
MAX_MEM_IN_USE="$(jq -r '.max_mem_in_use_bytes' "$RESULT_JSON")"
MODULE_COUNT="$(jq -r '.module_count' "$RESULT_JSON")"

note "measured: ok=$OK elapsed_seconds=$ELAPSED max_mem_in_use_bytes=$MAX_MEM_IN_USE module_count=$MODULE_COUNT"

if [ "$OK" != "true" ]; then
    report_fail "inference itself failed -- this is a real inference bug, not a cost regression. See stderr above."
fi

# Compare as integers where possible; elapsed_seconds needs float comparison.
if awk -v e="$ELAPSED" -v m="$MAX_SECONDS" 'BEGIN { exit !(e > m) }'; then
    report_fail "elapsed_seconds=$ELAPSED exceeds budget.max_elapsed_seconds=$MAX_SECONDS"
fi
if [ "$MAX_MEM_IN_USE" -gt "$MAX_MEM_BYTES" ]; then
    report_fail "max_mem_in_use_bytes=$MAX_MEM_IN_USE exceeds budget.max_mem_in_use_bytes=$MAX_MEM_BYTES"
fi

if [ "$RECORD_BASELINE" = "1" ] && [ "$fail" = "0" ]; then
    echo ""
    echo "=== Recording new baseline ==="
    STAMP="$(date -u +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || echo unknown)"
    jq --argjson elapsed "$ELAPSED" \
       --argjson mem "$MAX_MEM_IN_USE" \
       --argjson modules "$MODULE_COUNT" \
       --arg stamp "$STAMP" \
       '.baseline = {"elapsed_seconds": $elapsed, "max_mem_in_use_bytes": $mem, "module_count": $modules, "recorded_at": $stamp}
        | .history += [{"elapsed_seconds": $elapsed, "max_mem_in_use_bytes": $mem, "module_count": $modules, "recorded_at": $stamp}]' \
       "$BASELINE_FILE" > "$TMP/baseline-updated.json"
    mv "$TMP/baseline-updated.json" "$BASELINE_FILE"
    note "Baseline recorded in $BASELINE_FILE (review + commit)."
fi

echo ""
if [ "$fail" = "0" ]; then
    echo "=== PASS: inference cost within budget ==="
else
    echo "=== FAIL: inference cost regression detected ===" >&2
fi
exit "$fail"
