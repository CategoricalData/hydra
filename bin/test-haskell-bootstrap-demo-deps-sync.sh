#!/usr/bin/env bash
# Regression test for #670 — the bootstrap demo's Haskell dependency list
# silently drifting from heads/haskell/package.yaml (the hand-maintained
# root), which broke every `*-to-haskell` bootstrap cell when #666 added a
# `unix` dependency and missed the demo copy.
#
# Hermetic: does not touch dist/, does not build anything. Verifies:
#   1. --check exits 0 against the committed (in-sync) tree.
#   2. Planting drift in the demo file (deleting one dependency line) makes
#      --check fail.
#   3. Running the generator (no flags) resolves the drift and --check then
#      passes again.
#   4. The generator is idempotent: a second run makes no further changes.
#
# Restores the pre-test file contents on exit, pass or fail.
#
# Usage:
#   bin/test-haskell-bootstrap-demo-deps-sync.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
SYNC_SCRIPT="$HYDRA_ROOT_DIR/bin/lib/sync-haskell-bootstrap-demo-deps.py"
DEMO_PKG_YAML="$HYDRA_ROOT_DIR/demos/bootstrapping/resources/haskell/package.yaml"

PASS=0
FAIL=0

BACKUP="$(mktemp -t test-670-demo-deps.XXXXXX)"
cp "$DEMO_PKG_YAML" "$BACKUP"
_restore() {
    cp "$BACKUP" "$DEMO_PKG_YAML"
    rm -f "$BACKUP"
}
trap _restore EXIT

check() {
    local desc="$1" expect="$2"
    local rc=0
    python3 "$SYNC_SCRIPT" --check >/tmp/test-670-check.log 2>&1 || rc=$?
    if [ "$rc" = "$expect" ]; then
        echo "PASS: $desc (--check exit $rc)"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $desc (--check exit $rc, expected $expect)"
        cat /tmp/test-670-check.log
        FAIL=$((FAIL + 1))
    fi
}

echo "[1] Committed tree should already be in sync"
check "committed tree in sync" 0

echo "[2] Planting drift (removing the 'unix' dependency line)"
grep -v '^  - unix ' "$DEMO_PKG_YAML" > "$DEMO_PKG_YAML.tmp" && mv "$DEMO_PKG_YAML.tmp" "$DEMO_PKG_YAML"
check "drift detected" 1

echo "[3] Running the generator to resolve drift"
python3 "$SYNC_SCRIPT" >/tmp/test-670-sync.log 2>&1 || {
    echo "FAIL: generator exited nonzero"
    cat /tmp/test-670-sync.log
    FAIL=$((FAIL + 1))
}
check "drift resolved" 0

echo "[4] Idempotency: second run makes no further changes"
BEFORE_HASH=$(shasum -a 256 "$DEMO_PKG_YAML" | awk '{print $1}')
python3 "$SYNC_SCRIPT" >/tmp/test-670-sync2.log 2>&1
AFTER_HASH=$(shasum -a 256 "$DEMO_PKG_YAML" | awk '{print $1}')
if [ "$BEFORE_HASH" = "$AFTER_HASH" ]; then
    echo "PASS: idempotent rerun"
    PASS=$((PASS + 1))
else
    echo "FAIL: second run changed the file"
    FAIL=$((FAIL + 1))
fi

echo ""
echo "=== test-haskell-bootstrap-demo-deps-sync.sh: $PASS pass, $FAIL fail ==="

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
