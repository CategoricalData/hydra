#!/usr/bin/env bash
# #376 Stage 1 — cold-clone proof harness.
#
# Proves the json-driver cold seeder reproduces dist/haskell from dist/json
# on a genuinely COLD checkout (dist/haskell absent, no warm-worktree state
# reused). Never run the untrack verification on the warm worktree itself —
# git rm --cached leaves files on disk, which masks exactly the kind of
# breakage this harness exists to catch (#376 was reverted twice for this
# reason).
#
# Usage: scratchpad/cold-clone-proof.sh [--skip-ext-build-demo]
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

SKIP_EXT_DEMO=false
for arg in "$@"; do
    case "$arg" in
        --skip-ext-build-demo) SKIP_EXT_DEMO=true ;;
        *) echo "Unknown argument: $arg" >&2; exit 2 ;;
    esac
done

COLD_DIR="$(mktemp -d -t hydra-cold-clone-XXXXXX)"
echo "=== #376 cold-clone proof ==="
echo "  cold dir: $COLD_DIR"
echo ""
trap 'echo "(leaving cold dir in place for inspection: $COLD_DIR)"' EXIT

echo "[1/5] git archive HEAD -> cold dir (dist/haskell will be ABSENT after we remove it)..."
( cd "$REPO_ROOT" && git archive HEAD ) | tar -x -C "$COLD_DIR"

# The archive still carries tracked dist/haskell (Stage 3 hasn't run yet on
# this branch). Simulate the post-untrack cold-clone state by removing it,
# since the whole point of this harness is to prove the seed step alone can
# reproduce it from dist/json.
rm -rf "$COLD_DIR/dist/haskell"
echo "  removed dist/haskell from the cold checkout (simulating post-untrack clone)"

# The json-driver's headmods/ and other gitignored build artifacts don't
# ride along with git archive; stage the driver source itself (it IS
# tracked) — nothing extra needed, git archive already included it.

echo ""
echo "[2/5] Running cold-seed-dist-haskell.sh in the cold checkout..."
HYDRA_ROOT_DIR="$COLD_DIR" "$COLD_DIR/heads/haskell/json-driver/bin/cold-seed-dist-haskell.sh" \
    --repo-root "$COLD_DIR"

echo ""
echo "[3/5] Verifying per-package .hs counts against the tracked baseline..."
FAIL=0
# Baselines are git-tracked main-source .hs counts, EXCLUDING both the
# Overlay/ subtree (hand-written, copied not generated) AND the top-level
# Kernel.hs/Settings.hs overlay runtime files (gitignored, present on disk in
# a warm worktree but NOT tracked in git — `git ls-files` is the only
# reliable source of truth here; a plain `find` over a warm worktree
# overcounts by these 2 files for hydra-kernel). Verified via:
#   git ls-files "dist/haskell/<pkg>/src/main/haskell/*.hs" | grep -v /Overlay/ | wc -l
declare -A BASELINE=(
    [hydra-kernel]=240 [hydra-haskell]=10 [hydra-java]=14 [hydra-jvm]=1
    [hydra-python]=13 [hydra-scala]=6 [hydra-go]=3 [hydra-lisp]=5 [hydra-coq]=8
    [hydra-typescript]=6 [hydra-build]=3 [hydra-pg]=40 [hydra-rdf]=11
    [hydra-ext]=55 [hydra-wasm]=4 [hydra-bench]=3
)
TOTAL_EXPECTED=0
TOTAL_ACTUAL=0
for pkg in "${!BASELINE[@]}"; do
    expected="${BASELINE[$pkg]}"
    actual="$(find "$COLD_DIR/dist/haskell/$pkg/src/main/haskell" -name '*.hs' 2>/dev/null | wc -l | tr -d ' ')"
    TOTAL_EXPECTED=$((TOTAL_EXPECTED + expected))
    TOTAL_ACTUAL=$((TOTAL_ACTUAL + actual))
    if [ "$actual" != "$expected" ]; then
        echo "  MISMATCH: $pkg expected=$expected actual=$actual"
        FAIL=1
    else
        echo "  OK: $pkg = $actual"
    fi
done
echo "  TOTAL: expected=$TOTAL_EXPECTED actual=$TOTAL_ACTUAL"
if [ "$FAIL" != 0 ]; then
    echo "FAIL: per-package count mismatch (see above)" >&2
    exit 1
fi

echo ""
echo "[4/5] Verifying all 16 manifests were emitted..."
MANIFEST_FAIL=0
for pkg in $(python3 "$COLD_DIR/bin/lib/hydra-packages.py" list); do
    if [ ! -f "$COLD_DIR/dist/haskell/$pkg/package.yaml" ]; then
        echo "  MISSING manifest: $pkg"
        MANIFEST_FAIL=1
    fi
done
if [ "$MANIFEST_FAIL" != 0 ]; then
    echo "FAIL: missing package.yaml manifests (see above)" >&2
    exit 1
fi
echo "  OK: all 16 manifests present"

if [ "$SKIP_EXT_DEMO" = true ]; then
    echo ""
    echo "[5/5] Skipped (--skip-ext-build-demo)."
else
    echo ""
    echo "[5/5] On-demand build demo: stack build hydra-ext in its seeded dir..."
    ( cd "$COLD_DIR/dist/haskell/hydra-ext" && stack build )
    echo "  OK: hydra-ext built standalone against its generated stack.yaml extra-deps"
fi

echo ""
echo "=== cold-clone proof PASSED ==="
