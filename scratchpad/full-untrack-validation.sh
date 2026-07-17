#!/usr/bin/env bash
# #376 Stage 3 final validation — genuinely cold clone AFTER the untrack.
#
# Unlike cold-clone-proof.sh (which manually removed dist/haskell from an
# archived warm tree, simulating post-untrack state before the untrack
# actually happened), this runs AFTER the real `git rm --cached dist/haskell`
# commit, so `git archive HEAD` naturally produces a checkout with NO
# dist/haskell/ at all — no manual removal needed, no risk of the warm-tree
# masking trap that reverted #376 twice before.
#
# Validates: bin/sync.sh's cold-start detection seeds dist/haskell, the head
# builds, and `stack test` passes — end to end, from a genuinely cold clone.
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

COLD_DIR="$(mktemp -d -t hydra-post-untrack-XXXXXX)"
echo "=== #376 post-untrack cold-clone validation ==="
echo "  cold dir: $COLD_DIR"
echo ""

echo "[1/4] git archive HEAD -> cold dir..."
( cd "$REPO_ROOT" && git archive HEAD ) | tar -x -C "$COLD_DIR"

if [ -d "$COLD_DIR/dist/haskell" ]; then
    echo "FAIL: dist/haskell/ exists in a fresh git-archive checkout post-untrack!" >&2
    exit 1
fi
echo "  confirmed: dist/haskell/ genuinely absent (no manual removal needed)"

echo ""
echo "[2/4] Running bin/sync.sh --hosts haskell --targets haskell (cold-start should auto-seed)..."
( cd "$COLD_DIR" && bin/sync.sh --hosts haskell --targets haskell --no-tests )

if [ ! -d "$COLD_DIR/dist/haskell/hydra-kernel/src/main/haskell" ]; then
    echo "FAIL: dist/haskell/hydra-kernel not seeded by bin/sync.sh cold-start path" >&2
    exit 1
fi
echo "  OK: dist/haskell seeded by cold-start detection"

echo ""
echo "[3/4] Running stack test..."
( cd "$COLD_DIR/heads/haskell" && stack test )

echo ""
echo "[4/4] Running verify-distribution.sh (self-containment gate)..."
( cd "$COLD_DIR" && heads/haskell/bin/verify-distribution.sh )

echo ""
echo "=== post-untrack cold-clone validation PASSED ==="
echo "  cold dir left in place for inspection: $COLD_DIR"
