#!/usr/bin/env bash
# Regression test for #839: assemble_check_fresh / assemble_check_fresh_readonly must recompute the
# input digest from the current dist/json content, so a git operation (rebase/merge/checkout) that
# changes tracked dist/json sources without touching the gitignored build/ digests is not reported
# as "fresh". Synthetic package in a temp dir; needs only bash, jq, sha256sum.

set -euo pipefail

HYDRA_ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export HYDRA_ROOT_DIR
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

TMP="$(mktemp -d -t test-839-XXXXXX)"
trap 'rm -rf "$TMP"' EXIT

PKG=hydra-test839
JSON_ROOT="$TMP/dist/json"
SRC="$JSON_ROOT/$PKG/src/main/json"
OUT="$TMP/dist/haskell/$PKG/src/main/haskell"
IN_DIGEST="$JSON_ROOT/$PKG/build/main/digest.json"
OUT_DIGEST="$TMP/dist/haskell/$PKG/build/main/digest.json"

mkdir -p "$SRC" "$OUT" "$(dirname "$OUT_DIGEST")"
echo '{"mainModules":["test839.mod"]}' > "$SRC/manifest.json"
echo '{"v":1}' > "$SRC/mod.json"
echo 'generated' > "$OUT/Mod.hs"

"$HYDRA_ROOT_DIR/bin/digest.sh" refresh-input --package "$PKG" --dist-json-root "$JSON_ROOT" >/dev/null
# Make the digest look Phase-1 shaped: extra entries bash refresh-input can't reproduce.
jq '.moduleHashes += [{key:"test839.mod",value:"merkle"},{key:"test839.dsl",value:"x"}]' "$IN_DIGEST" \
    > "$IN_DIGEST.new" && mv "$IN_DIGEST.new" "$IN_DIGEST"
PHASE1_SUM="$(sha256sum "$IN_DIGEST")"
"$HYDRA_ROOT_DIR/bin/digest.sh" refresh --inputs "$IN_DIGEST" --output-dir "$OUT" \
    --output-digest "$OUT_DIGEST" >/dev/null

fail() { echo "FAIL: $*" >&2; exit 1; }

assemble_check_fresh "$IN_DIGEST" "$OUT" "$OUT_DIGEST" >/dev/null \
    || fail "unchanged sources should be fresh"
assemble_check_fresh_readonly "$IN_DIGEST" "$OUT" "$OUT_DIGEST" >/dev/null \
    || fail "unchanged sources should be fresh (readonly)"

[ "$(sha256sum "$IN_DIGEST")" = "$PHASE1_SUM" ] \
    || fail "a current Phase-1-shaped input digest must not be rewritten"

# Simulate a rebase: tracked JSON changes, build/ digests untouched.
echo '{"v":2}' > "$SRC/mod.json"

if assemble_check_fresh_readonly "$IN_DIGEST" "$OUT" "$OUT_DIGEST" >/dev/null 2>&1; then
    fail "changed dist/json content reported fresh by assemble_check_fresh_readonly"
fi
[ "$(sha256sum "$IN_DIGEST")" = "$PHASE1_SUM" ] || fail "readonly probe rewrote the input digest"
if assemble_check_fresh "$IN_DIGEST" "$OUT" "$OUT_DIGEST" >/dev/null 2>&1; then
    fail "changed dist/json content reported fresh by assemble_check_fresh"
fi

# The real check rewrote the stale digest; the new digest is consistent.
grep -q '"jsonContent:mod.json"' "$IN_DIGEST" || fail "stale digest was not refreshed by assemble_check_fresh"
"$HYDRA_ROOT_DIR/bin/digest.sh" refresh --inputs "$IN_DIGEST" --output-dir "$OUT" \
    --output-digest "$OUT_DIGEST" >/dev/null
assemble_check_fresh "$IN_DIGEST" "$OUT" "$OUT_DIGEST" >/dev/null || fail "not fresh after re-stamping"

# Test source set: Phase-1 test digests carry no jsonContent keys, so staleness is by mtime.
TSRC="$JSON_ROOT/$PKG/src/test/json"; TDIG="$JSON_ROOT/$PKG/build/test/digest.json"
mkdir -p "$TSRC" "$(dirname "$TDIG")"; echo '{"t":1}' > "$TSRC/t.json"
echo '{"digestFormatVersion":2,"moduleFormatVersion":1,"dependencyHashes":[],"moduleHashes":[{"key":"u","value":"h"}]}' > "$TDIG"
touch -d '1 hour ago' "$TSRC/t.json"
assemble_input_digest_current "$TDIG" || fail "test digest newer than sources should be current"
touch "$TSRC/t.json"; sleep 1; touch "$TSRC/t.json"
touch -d '1 minute ago' "$TDIG"
assemble_input_digest_current "$TDIG" && fail "test digest older than sources should be stale"

echo "PASS: #839 assemble freshness gate sees on-disk dist/json changes"
