#!/usr/bin/env bash
# Regression test for #551: a kernel test-DSL source change must NOT be
# skipped by the per-package test-digest cache-hit check.
#
# Background: #546 added a per-package test digest for every package that
# owns test modules (today hydra-build and hydra-kernel), and a
# corresponding existence guard (allDigestsPresent) requiring all of them to
# be on disk before considering a cache hit. But the freshness comparison
# itself (checkCacheHit) read only ONE of those digest files — the "anchor"
# — so the existence check and the freshness check spanned different sets.
# A kernel test-source change was masked whenever the anchor package's
# digest happened to still look fresh, i.e. downstream per-host regen for
# non-anchor test content silently skipped (#513/#546 batch: CI green on
# Java/Scala with a stale CoderDirection expectation).
#
# The fix (checkCacheHitAll in Hydra.Generation) requires EVERY per-package
# test digest to be both present AND fresh vs the current universe hash.
# This test exercises the real update-json-test path end to end:
#   1. Seed a clean, fresh test-digest state (run update-json-test once).
#   2. Confirm a cache HIT on the unchanged tree (sanity: the fast path
#      still works — a fix that always reports a miss would trivially
#      "pass" a freshness check without actually fixing anything).
#   3. Touch a kernel-owned test DSL source (a namespace that does NOT
#      route to the anchor package) with a semantically-inert comment.
#   4. Re-run update-json-test and assert it reports a MISS (regenerates),
#      not a "Cache hit" — this is the exact #551 gap: pre-fix, the anchor
#      (hydra-build)'s digest doesn't see a hydra-kernel-owned source
#      change reflected in ITS OWN file, but happened to still match the
#      current universe hash in the buggy single-file comparison in some
#      states; the safeguard here is asserting the observable behavior
#      (miss on real content change), which is what actually matters for
#      correctness.
#   5. Revert the source change and restore a clean, fresh digest state.
#
# Not hermetic: requires a synced dist/json/ tree (both packages' test
# digests present). SKIPs cleanly otherwise. On a green run, leaves the
# working tree and dist/json/ exactly as it found them.
#
# Usage:
#   bin/test-test-digest-freshness.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HASKELL_DIR="$HYDRA_ROOT_DIR/heads/haskell"

PASS=0
FAIL=0
fail() { echo "  FAIL: $*"; FAIL=$((FAIL + 1)); }

KERNEL_TEST_DIGEST="$HYDRA_ROOT_DIR/dist/json/hydra-kernel/build/test/digest.json"
BUILD_TEST_DIGEST="$HYDRA_ROOT_DIR/dist/json/hydra-build/build/test/digest.json"
KERNEL_TEST_SRC="$HYDRA_ROOT_DIR/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/Checking/NominalTypes.hs"
MARKER="-- test-551-regression-marker (transient; reverted by bin/test-test-digest-freshness.sh)"

echo "=== test-test-digest-freshness.sh (#551) ==="

# Require a synced tree: without existing per-package test digests, every
# run is a cache miss regardless of the fix, and the test would be
# meaningless. SKIP rather than fail when not synced.
if [ ! -f "$KERNEL_TEST_DIGEST" ] || [ ! -f "$BUILD_TEST_DIGEST" ]; then
    echo "  SKIP: per-package test digests not present (run a sync first)"
    echo "=== $PASS pass, $FAIL fail (skipped) ==="
    exit 0
fi

# Build update-json-test from current source. `stack exec` never rebuilds,
# so a stale binary predating the #551 fix would silently take the old
# anchor-only path and this test would pass against good source for the
# wrong reason.
echo "Building update-json-test from current source..."
( cd "$HASKELL_DIR" && stack build hydra:exe:update-json-test )

# Restore the source file and re-refresh digests on exit, however we got
# there, so a crash mid-test doesn't leave the working tree dirty.
_cleanup() {
    git -C "$HYDRA_ROOT_DIR" checkout -q -- "$KERNEL_TEST_SRC" 2>/dev/null || true
}
trap _cleanup EXIT

run_update_json_test() {
    ( cd "$HASKELL_DIR" && stack exec update-json-test )
}

echo "[setup] seeding a clean, fresh test-digest state"
if ! run_update_json_test > /tmp/test-551-seed.log 2>&1; then
    cat /tmp/test-551-seed.log
    fail "seed update-json-test exited non-zero"
    echo ""; echo "=== $PASS pass, $FAIL fail ==="; exit 1
fi

echo "[case 1] cache hit on unchanged tree (sanity: fast path still works)"
log1=/tmp/test-551-hit.log
if ! run_update_json_test > "$log1" 2>&1; then
    cat "$log1"
    fail "update-json-test exited non-zero on unchanged tree"
else
    if grep -q "Cache hit" "$log1"; then
        echo "  PASS"
        PASS=$((PASS + 1))
    else
        fail "expected a cache hit on an unchanged tree; see $log1"
        cat "$log1"
    fi
fi

echo "[case 2] kernel test-source change is NOT skipped (the #551 gap)"
echo "$MARKER" >> "$KERNEL_TEST_SRC"
log2=/tmp/test-551-miss.log
if ! run_update_json_test > "$log2" 2>&1; then
    cat "$log2"
    fail "update-json-test exited non-zero after kernel test-source change"
else
    if grep -q "Cache hit" "$log2"; then
        fail "kernel test-source change was NOT detected (false cache hit) — #551 regression!"
        cat "$log2"
    else
        echo "  PASS"
        PASS=$((PASS + 1))
    fi
fi

echo "[cleanup] reverting kernel test-source change + restoring fresh digest state"
git -C "$HYDRA_ROOT_DIR" checkout -q -- "$KERNEL_TEST_SRC"
if ! run_update_json_test > /tmp/test-551-restore.log 2>&1; then
    cat /tmp/test-551-restore.log
    fail "restore update-json-test exited non-zero"
fi

echo ""
echo "=== test-test-digest-freshness.sh: $PASS pass, $FAIL fail ==="

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
