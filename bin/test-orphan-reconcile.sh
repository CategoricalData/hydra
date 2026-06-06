#!/usr/bin/env bash
# Regression test for #393 cross-namespace rename orphan reconcile.
#
# Background: when a DSL module is renamed across namespaces (e.g.
# hydra.eval.lib.lists -> hydra.lib.defaults.lists), the target output of
# the OLD namespace dir (dist/<lang>/.../hydra/eval/lib/) is left behind.
# The generator-side prune (#357) would remove it, but the prune only runs
# inside assemble-distribution.sh Step 1, which is gated by digest-check
# fresh. The renamed-to module's content is unchanged, so the gate reports
# a cache hit and the prune never runs — the orphan lingers indefinitely
# (it broke a Phase 5 Java rollup compile after #368; see issue #393).
#
# The fix makes `digest-check fresh` orphan-aware: when inputs, generator,
# and all RECORDED outputs match but the output dir holds EXTRA files, it
# deletes the orphans in place (the recorded output set is the keep-set),
# refreshes the output digest, and still reports a hit. This test exercises
# that gated path directly via digest-check (the existing
# test-stale-output-prune.sh bypasses the gate by calling bootstrap-from-json
# with --prune-stale, so it can't catch this).
#
# Hermetic: builds its own input/output digest pair over a temp dir with a
# fixed generator stamp; no coder run, no real dist tree needed.
#
# Usage:
#   bin/test-orphan-reconcile.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

PASS=0
FAIL=0

fail() { echo "  FAIL: $*"; FAIL=$((FAIL + 1)); }

# Fixed stamp so refresh and fresh agree on the generator identity without
# depending on the real generator sources.
export HYDRA_GENERATOR_STAMP="test-orphan-reconcile-stamp"

# Build digest-check from current source before any `stack exec`. `stack exec`
# runs whatever binary is already in .stack-work and never rebuilds, so a stale
# executable predating the #393 orphan-reconcile path silently takes the old
# "cache hit; skipping work" branch and the orphan survives — a false failure
# against good source. The real pipeline builds the exe in sync-haskell.sh
# before exec; mirror that here.
echo "Building digest-check from current source..."
( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack build hydra:exe:digest-check )

WORK="$(mktemp -d -t hydra-orphan-reconcile.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

OUT_DIR="$WORK/out"            # stands in for dist/<lang>/<pkg>/src/main/<lang>
BUILD_DIR="$WORK/build"        # stands in for dist/<lang>/<pkg>/build/main
INPUT_DIGEST="$WORK/input-digest.json"
OUTPUT_DIGEST="$BUILD_DIR/digest.json"

mkdir -p "$OUT_DIR/hydra/lib/defaults" "$OUT_DIR/hydra/dsl" "$BUILD_DIR"

# The "renamed-to" generated file and a stand-in hand-written runtime file.
# Both legitimately belong to the package and must survive reconcile.
echo "renamed-to generated content" > "$OUT_DIR/hydra/lib/defaults/Lists.txt"
echo "hand-written runtime content" > "$OUT_DIR/hydra/dsl/Runtime.txt"

# Minimal but valid input digest (per Hydra.Digest serializePerPackageDigest).
cat > "$INPUT_DIGEST" <<'EOF'
{
  "digestFormatVersion": 1,
  "moduleFormatVersion": 1,
  "selfHash": "deadbeef",
  "hashes": {
    "hydra.lib.defaults.lists": "aaaa1111"
  }
}
EOF

run_digest_check() {
    local mode="$1"
    ( cd "$HYDRA_ROOT_DIR/heads/haskell" && \
      stack exec digest-check -- "$mode" \
        --inputs "$INPUT_DIGEST" \
        --output-dir "$OUT_DIR" \
        --output-digest "$OUTPUT_DIGEST" )
}

echo "=== test-orphan-reconcile.sh (#393) ==="

# Step 1: record a clean digest over the two legitimate files.
echo "[setup] refresh: recording clean output digest"
if ! run_digest_check refresh > "$WORK/refresh.log" 2>&1; then
    cat "$WORK/refresh.log"
    fail "initial refresh failed"
    echo ""; echo "=== $PASS pass, $FAIL fail ==="; exit 1
fi

# Sanity: a plain fresh check on the clean tree must be a hit (exit 0).
echo "[case 1] fresh on clean tree -> hit"
if run_digest_check fresh > "$WORK/fresh-clean.log" 2>&1; then
    if grep -q "cache hit" "$WORK/fresh-clean.log" && ! grep -q "reconcil" "$WORK/fresh-clean.log"; then
        echo "  PASS"; PASS=$((PASS + 1))
    else
        cat "$WORK/fresh-clean.log"; fail "clean tree should be a plain hit, no reconcile"
    fi
else
    cat "$WORK/fresh-clean.log"; fail "clean tree should be a hit (exit 0)"
fi

# Step 2: drop a cross-namespace orphan, mimicking the renamed-away dir.
ORPHAN="$OUT_DIR/hydra/eval/lib/Lists.txt"
mkdir -p "$(dirname "$ORPHAN")"
echo "stale renamed-away content (Context cx)" > "$ORPHAN"

echo "[case 2] fresh with orphan present -> reconcile, hit, orphan removed"
if run_digest_check fresh > "$WORK/fresh-orphan.log" 2>&1; then
    ok=1
    grep -q "reconciling" "$WORK/fresh-orphan.log" || { cat "$WORK/fresh-orphan.log"; fail "expected reconcile message"; ok=0; }
    [ ! -f "$ORPHAN" ] || { fail "orphan file survived reconcile: $ORPHAN"; ok=0; }
    [ ! -d "$OUT_DIR/hydra/eval" ] || echo "  WARN: emptied orphan dir survived (hydra/eval)"
    [ -f "$OUT_DIR/hydra/lib/defaults/Lists.txt" ] || { fail "legitimate generated file was deleted"; ok=0; }
    [ -f "$OUT_DIR/hydra/dsl/Runtime.txt" ] || { fail "runtime file was deleted"; ok=0; }
    # Refreshed digest must no longer reference the orphan.
    if grep -q "eval/lib/Lists.txt" "$OUTPUT_DIGEST"; then
        fail "refreshed digest still references the orphan"; ok=0
    fi
    if [ "$ok" = 1 ]; then echo "  PASS"; PASS=$((PASS + 1)); fi
else
    cat "$WORK/fresh-orphan.log"; fail "fresh-with-orphan should reconcile and exit 0"
fi

# Step 3: a second fresh must now be a plain hit (orphan gone, digest clean).
echo "[case 3] fresh after reconcile -> plain hit"
if run_digest_check fresh > "$WORK/fresh-after.log" 2>&1; then
    if grep -q "cache hit" "$WORK/fresh-after.log" && ! grep -q "reconcil" "$WORK/fresh-after.log"; then
        echo "  PASS"; PASS=$((PASS + 1))
    else
        cat "$WORK/fresh-after.log"; fail "post-reconcile tree should be a plain hit"
    fi
else
    cat "$WORK/fresh-after.log"; fail "post-reconcile tree should be a hit (exit 0)"
fi

echo ""
echo "=== test-orphan-reconcile.sh: $PASS pass, $FAIL fail ==="
if [ "$FAIL" -gt 0 ]; then exit 1; else exit 0; fi
