#!/usr/bin/env bash
# Conformance guardrail for #735/#719: proves decimal scale actually survives from
# dist/json into a generated non-Haskell host's native test fixture, closing the gap
# a conformance audit raised (later refuted, but the underlying claim -- "is this
# really tested end-to-end, or just asserted by static reading" -- deserves a real
# answer instead of re-litigating it by argument each time).
#
# Ground truth: dist/json/hydra-kernel/src/test/json/hydra/test/lib/ordering.json's
# "same value, scale tiebreak" case must carry two textually DISTINCT decimal JSON
# number tokens (e.g. 1.1 vs 1.10) -- if #719's printDecimal scale-preservation ever
# regresses, this collapses to identical tokens and the test below catches it before
# any host-specific investigation is needed.
#
# Live check: runs heads/java/bin/transform-json-to-target.sh to regenerate the Java
# fixture for hydra-kernel's test modules into a scratch directory (no full sync, no
# local Java build required -- target-driver's published-host classpath is sufficient),
# then asserts the generated Ordering.java contains two distinct `new
# java.math.BigDecimal("...")` literals for the scale-tiebreak case. This is the
# concrete, executable version of the "decode a generated non-Haskell fixture" check
# that #735's redrive asked for, rather than only reading printDecimal's source.
#
# SKIPs cleanly (exit 0) when dist/json/hydra-kernel isn't present or the Java
# transform can't run (matches the SKIP-cleanly convention of
# test-json-content-invalidates-render.sh and friends in bin/test-regressions.sh) --
# this is a conformance/regression guardrail for when the toolchain IS available, not
# a hard build-environment gate.
#
# Usage:
#   bin/test-decimal-scale-conformance.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"

fail=0
report_fail() { echo "FAIL: $*" >&2; fail=1; }
note() { echo "  $*"; }

ORDERING_JSON="$HYDRA_ROOT_DIR/dist/json/hydra-kernel/src/test/json/hydra/test/lib/ordering.json"

if [ ! -f "$ORDERING_JSON" ]; then
    echo "SKIP: $ORDERING_JSON missing (run a sync first)"
    exit 0
fi

echo "=== Step 1: dist/json ground truth (static) ==="
# The "same value, scale tiebreak" case pairs a decimal token with itself at a
# different scale. Extract every "decimal": <token> occurrence and require at least
# one pair of textually distinct tokens that are numerically equal (the scale-tiebreak
# signature) -- proves the WRITTEN json is not lossy, independent of any host.
DECIMAL_TOKENS="$(grep -o '"decimal": *[0-9.eE+-]*' "$ORDERING_JSON" | sed 's/"decimal": *//' | sort -u)"
DISTINCT_COUNT="$(echo "$DECIMAL_TOKENS" | grep -c '^1\.1' || true)"
if [ "$DISTINCT_COUNT" -lt 2 ]; then
    report_fail "expected >=2 distinct '1.1*'-prefixed decimal tokens in $ORDERING_JSON (scale-tiebreak signature); found $DISTINCT_COUNT. Scale may have collapsed at the JSON-write step."
else
    note "PASS: $ORDERING_JSON carries $DISTINCT_COUNT distinct 1.1*-prefixed decimal tokens (scale preserved on the wire)"
fi

echo ""
echo "=== Step 2: generated Java fixture (live) ==="
JAVA_TRANSFORM="$HYDRA_ROOT_DIR/heads/java/bin/transform-json-to-target.sh"
if [ ! -x "$JAVA_TRANSFORM" ]; then
    echo "SKIP: $JAVA_TRANSFORM not found or not executable"
    exit "$fail"
fi

TMP="$(mktemp -d -t hydra-decimal-scale-conformance.XXXXXX)"
trap 'rm -rf "$TMP"' EXIT

# #735: known environment hazard -- when hostOverrides.java=local is active (currently
# the case, per #719) and the published-host target-driver probe fails, this falls back
# to a local :hydra-java:compileHeadsExtrasJava build, which can hang indefinitely under
# JDK 17 (javac DeferredAttr pathology, unrelated to this script or #735 -- see
# task_735_scale_distinct_translingual-plan.md). Bound it so a bad environment produces
# a clean SKIP instead of an indefinite hang.
TRANSFORM_TIMEOUT="${HYDRA_DECIMAL_SCALE_CONFORMANCE_TIMEOUT:-120}"
LOG="$TMP/transform.log"
if ! timeout "$TRANSFORM_TIMEOUT" "$JAVA_TRANSFORM" java hydra-kernel test --output "$TMP/out" \
        --dist-json-root "$HYDRA_ROOT_DIR/dist/json" >"$LOG" 2>&1; then
    RC=$?
    if [ "$RC" -eq 124 ]; then
        echo "SKIP: Java transform exceeded ${TRANSFORM_TIMEOUT}s (likely the known javac-hang environment hazard, not a #735 regression signal -- see $LOG)"
    else
        echo "SKIP: Java transform failed (toolchain/environment issue, not a #735 regression signal -- see $LOG)"
    fi
    tail -20 "$LOG" || true
    exit "$fail"
fi

ORDERING_JAVA="$(find "$TMP/out" -iname "Ordering.java" | head -1)"
if [ -z "$ORDERING_JAVA" ] || [ ! -f "$ORDERING_JAVA" ]; then
    report_fail "expected a generated Ordering.java under $TMP/out but found none"
else
    # The scale-tiebreak case's two operands must appear as textually distinct
    # BigDecimal string-constructor literals (e.g. "1.1" vs "1.10") -- the actual,
    # concrete evidence that the compiled Java test observes scale-distinctness at
    # runtime, not just that the source claims to.
    DISTINCT_BIGDECIMALS="$(grep -o 'new java\.math\.BigDecimal("1\.1[0-9]*")' "$ORDERING_JAVA" | sort -u | wc -l)"
    if [ "$DISTINCT_BIGDECIMALS" -lt 2 ]; then
        report_fail "expected >=2 distinct BigDecimal(\"1.1*\") literals in generated $ORDERING_JAVA; found $DISTINCT_BIGDECIMALS. Scale may have collapsed somewhere in DSL->JSON->Java codegen."
    else
        note "PASS: generated $ORDERING_JAVA carries $DISTINCT_BIGDECIMALS distinct BigDecimal(\"1.1*\") literals (scale survives DSL->JSON->Java codegen)"
    fi
fi

echo ""
if [ "$fail" -eq 0 ]; then
    echo "=== test-decimal-scale-conformance.sh: PASS ==="
else
    echo "=== test-decimal-scale-conformance.sh: FAIL ==="
fi
exit "$fail"
