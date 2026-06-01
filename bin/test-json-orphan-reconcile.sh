#!/usr/bin/env bash
# Regression test for #405: dist/json/<pkg> orphan reconcile.
#
# Background: the #357/#393 prune removes stale generated outputs for the
# per-language dist/<lang> trees (which record an output keep-set in a v2
# digest that `digest-check fresh` reconciles against), but NOT for the
# dist/json/<pkg> tree. A DSL/type module dropped from the emission set left
# its .json behind forever — e.g. dist/json/hydra-kernel/.../hydra/dsl/classes.json
# (a stale hydra.phantoms-era wrapper) survived a full digest-busted sync.
#
# The fix makes the JSON write path itself orphan-aware: after both write
# passes (type/term + DSL wrappers) finish, update-json-main reconciles each
# reconciled package's src/main/json against the in-memory emission set as
# keep-set, deleting anything else. This test exercises that real path by
# planting orphans into a freshly-synced JSON tree and re-running
# update-json-main.
#
# Properties asserted:
#   1. A planted orphan .json under a single-writer package (hydra-kernel) is
#      DELETED.
#   2. A real module's .json and the per-package manifest.json SURVIVE
#      (keep-set + manifest protection).
#   3. A planted .json under a SKIPPED package (hydra-java, owned by the
#      native generator) SURVIVES (the Haskell reconcile must not touch it).
#
# The DSL pass — and therefore the reconcile — runs unconditionally in
# update-json-main, so this works on an already-synced tree (main inference
# is a cache hit) without a full multi-minute regen.
#
# Not hermetic: it runs against the real dist/json tree. It only ADDS
# synthetic orphans (which the reconcile is expected to remove) and removes
# its own hydra-java sentinel afterward, so a green run leaves dist/json as it
# found it. SKIPs cleanly if no synced tree is present.
#
# Usage:
#   bin/test-json-orphan-reconcile.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HASKELL_DIR="$HYDRA_ROOT_DIR/heads/haskell"
JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"

PASS=0
FAIL=0
fail() { echo "  FAIL: $*"; FAIL=$((FAIL + 1)); }

KERNEL_MAIN="$JSON_ROOT/hydra-kernel/src/main/json"
JAVA_MAIN="$JSON_ROOT/hydra-java/src/main/json"

echo "=== test-json-orphan-reconcile.sh (#405) ==="

# Require a synced tree: the reconcile keeps exactly the emission set, so
# without real module JSON on disk it would (correctly) delete everything and
# the test would be meaningless. SKIP rather than fail when not synced.
if [ ! -f "$KERNEL_MAIN/manifest.json" ] || [ ! -f "$KERNEL_MAIN/hydra/core.json" ]; then
    echo "  SKIP: $KERNEL_MAIN not synced (run a sync first)"
    echo "=== $PASS pass, $FAIL fail (skipped) ==="
    exit 0
fi

# Build update-json-main from current source before exec. `stack exec` never
# rebuilds, so a stale binary predating the #405 reconcile would silently take
# the old no-prune path and the orphan would survive — a false failure against
# good source. The real pipeline builds it in sync-haskell.sh before exec.
echo "Building update-json-main from current source..."
( cd "$HASKELL_DIR" && stack build hydra:exe:update-json-main )

# Planted artifacts.
KERNEL_ORPHAN="$KERNEL_MAIN/hydra/dsl/classes.json"          # mimics the real #405 orphan
KERNEL_ORPHAN_NESTED="$KERNEL_MAIN/hydra/eval/lib/lists.json" # renamed-away namespace dir
JAVA_SENTINEL="$JAVA_MAIN/hydra/zzz_test_405_sentinel.json"   # in a SKIPPED package
KERNEL_REAL="$KERNEL_MAIN/hydra/core.json"                    # must survive
KERNEL_MANIFEST="$KERNEL_MAIN/manifest.json"                  # must survive

# Clean up the skip-package sentinel on exit (it is NOT pruned, so we own it).
cleanup() {
    rm -f "$JAVA_SENTINEL"
    # Best-effort: if the test aborted before the reconcile ran, drop our
    # planted kernel orphans too so we don't leave the tree dirty.
    rm -f "$KERNEL_ORPHAN" "$KERNEL_ORPHAN_NESTED" 2>/dev/null || true
    rmdir "$KERNEL_MAIN/hydra/eval/lib" "$KERNEL_MAIN/hydra/eval" 2>/dev/null || true
}
trap cleanup EXIT

# Plant orphans + a skip-package sentinel.
echo "[setup] planting orphan + sentinel files"
echo '{"orphan":"stale hydra.phantoms-era dsl wrapper"}' > "$KERNEL_ORPHAN"
mkdir -p "$(dirname "$KERNEL_ORPHAN_NESTED")"
echo '{"orphan":"renamed-away namespace dir"}' > "$KERNEL_ORPHAN_NESTED"
mkdir -p "$(dirname "$JAVA_SENTINEL")"
echo '{"sentinel":"must survive — hydra-java is native-owned, reconcile skips it"}' > "$JAVA_SENTINEL"

# Run the real JSON write path. The DSL pass + reconcile run unconditionally;
# main inference is a cache hit on an already-synced tree.
echo "[run] update-json-main"
if ! ( cd "$HASKELL_DIR" && stack exec update-json-main ) > /tmp/test-405-run.log 2>&1; then
    cat /tmp/test-405-run.log
    fail "update-json-main exited non-zero"
    echo ""; echo "=== $PASS pass, $FAIL fail ==="; exit 1
fi

echo "[case 1] planted kernel orphans pruned"
ok=1
[ ! -f "$KERNEL_ORPHAN" ]        || { fail "orphan survived: $KERNEL_ORPHAN"; ok=0; }
[ ! -f "$KERNEL_ORPHAN_NESTED" ] || { fail "nested orphan survived: $KERNEL_ORPHAN_NESTED"; ok=0; }
[ "$ok" = 1 ] && { echo "  PASS"; PASS=$((PASS + 1)); }

echo "[case 2] real module JSON + manifest.json survive"
ok=1
[ -f "$KERNEL_REAL" ]     || { fail "real module deleted: $KERNEL_REAL"; ok=0; }
[ -f "$KERNEL_MANIFEST" ] || { fail "manifest.json deleted (protect-set failed): $KERNEL_MANIFEST"; ok=0; }
[ "$ok" = 1 ] && { echo "  PASS"; PASS=$((PASS + 1)); }

echo "[case 3] skip-package (hydra-java) sentinel survives"
if [ -f "$JAVA_SENTINEL" ]; then
    echo "  PASS"; PASS=$((PASS + 1))
else
    fail "sentinel in skipped package was deleted: $JAVA_SENTINEL"
fi

echo ""
echo "=== test-json-orphan-reconcile.sh: $PASS pass, $FAIL fail ==="
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
