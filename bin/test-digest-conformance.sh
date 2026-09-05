#!/usr/bin/env bash
# Conformance test for the digest executor promotion (#416, Design D).
#
# bin/digest.sh is the host-FREE promotion of `digest-check`'s two hot
# subcommands (`fresh` / `refresh`): pure-bash sha256sum hashing + jq semantic
# digest r/w + the #393 orphan reconcile. This test asserts bin/digest.sh
# behaves IDENTICALLY to the authoritative `digest-check` oracle across the
# freshness matrix, so the bash port can never silently drift from the spec.
#
# The oracle here is the ACTUAL Haskell `digest-check` exec (Hydra.Digest +
# Hydra.DigestFormat) — unlike the assembly-plan conformance test (two pure-bash
# encodings of one DSL plan), digest freshness is a runtime behavior, so the
# oracle is the running tool. The DSL remains a TEST-TIME oracle: the shipped
# BUILD stops calling `stack exec digest-check` (bin/digest.sh replaces it);
# only this test invokes it, to prove equivalence.
#
# CONTRACT (416-digest-check-classification.md): equivalence is SEMANTIC, not
# byte-level. The Haskell writer uses a width-adaptive AST pretty-printer; both
# digest paths are gitignored build caches, never byte-diffed across commits,
# and bin/lib/batch-cache.sh already reads them via stdlib json.load. So this
# test compares the DECODED digest model (keys, hash values, generator,
# selfHash, deps, kinds) + the fresh/refresh VERDICT (exit code) + the
# orphan-delete DECISION (which files survive) — NOT `diff` of raw bytes.
#
# Oracle gating: the oracle (`stack exec digest-check`) is OFF by default and is
# NEVER probed via stack — any stack invocation touches the shared ~/.stack
# package DB and can collide with a concurrent sibling-worktree Haskell build
# (the repo's link-race hazard). By default the test runs bin/digest.sh's own
# internal-consistency checks (exit 0), matching the "needs a toolchain; runs
# self-checks otherwise" pattern of the sibling harnesses. The full oracle
# comparison runs ONLY when opted in with HYDRA_DIGEST_ORACLE=1, which the caller
# sets only while holding the Haskell build slot (staging-granted).
#
# Wired into bin/test-regressions.sh. Exit 0 = conformant (or cleanly skipped);
# nonzero = drift.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"
export HYDRA_ROOT_DIR

DIGEST_SH="$HYDRA_ROOT_DIR/bin/digest.sh"

# Deterministic generation env so both tools stamp identically. (In the real
# build these come from export_generation_env; here we pin them.)
export HYDRA_GENERATOR_STAMP="conformance-stamp-0001"
export HYDRA_GENERATION_MODE="published"
export HYDRA_GENERATION_HOST="haskell"
export HYDRA_GENERATION_HYDRA_VERSION="0.0.0-test"
export HYDRA_GENERATION_REVISION="deadbeef-test"
export HYDRA_GENERATION_TIMESTAMP="2020-01-01T00:00:00Z"

TMP="$(mktemp -d -t hydra-digest-conformance.XXXXXX)"
trap 'rm -rf "$TMP"' EXIT

fail=0
checks=0
note() { echo "  $*"; }
report_fail() { echo "DRIFT: $*" >&2; fail=1; }

# ---------------------------------------------------------------------------
# Oracle gating. The oracle is `stack exec digest-check`. Running ANY `stack`
# command touches the shared ~/.stack package DB and can collide with a
# concurrent Haskell build in a sibling worktree (the repo's link-race hazard),
# so the oracle path is OFF by default and NEVER probed via stack. It runs ONLY
# when explicitly opted in with HYDRA_DIGEST_ORACLE=1 — set that only when you
# hold the Haskell build slot (staging-granted). Default (test-regressions.sh,
# CI) runs bin/digest.sh's self-consistency checks alone; the sibling
# regression harnesses follow the same "needs a toolchain; SKIPs cleanly
# otherwise" convention.
ORACLE=0
if [ "${HYDRA_DIGEST_ORACLE:-0}" = "1" ]; then
    # Opted in: require the exe already built (stack exec won't rebuild). Locate
    # it on disk WITHOUT invoking stack, to avoid any package-DB touch during the
    # probe. If it's genuinely absent, fail loudly — the caller asked for the
    # oracle but there's nothing to run.
    if find "$HYDRA_ROOT_DIR/heads/haskell/.stack-work" -type f -name digest-check 2>/dev/null | grep -q .; then
        ORACLE=1
    else
        echo "test-digest-conformance: HYDRA_DIGEST_ORACLE=1 but no built digest-check exe found;" >&2
        echo "  build it (with a stack slot) via: (cd heads/haskell && stack build hydra:exe:digest-check)" >&2
        exit 1
    fi
fi

oracle() {
    # Run the Haskell oracle. Args: the digest-check CLI (fresh/refresh + flags).
    ( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack exec digest-check -- "$@" )
}

# ---------------------------------------------------------------------------
# Semantic digest comparison: decode both JSON files and compare the normalized
# model. jq with -S sorts object keys; we canonicalize the {key,value} arrays by
# sorting on .key so map order can't cause a false diff. Timestamps/versions are
# part of the model and MUST match (both tools read the same pinned env).
canon_digest() {
    # $1 = digest.json path. Emits a canonical JSON string, or "MISSING".
    local f="$1"
    [ -f "$f" ] || { echo "MISSING"; return 0; }
    jq -S '
      # sort every {key,value} array field by key for order-independence
      def sortkv(f): if has(f) then .[f] |= sort_by(.key) else . end;
      sortkv("dependencyHashes") | sortkv("moduleHashes")
        | sortkv("inputs") | sortkv("outputs")
    ' "$f" 2>/dev/null || echo "UNPARSEABLE"
}

assert_digests_equiv() {
    # $1 = label, $2 = bash digest, $3 = oracle digest
    local label="$1" a b
    a="$(canon_digest "$2")"
    b="$(canon_digest "$3")"
    if [ "$a" != "$b" ]; then
        report_fail "$label: digest model differs (bash vs oracle)"
        diff <(printf '%s\n' "$a") <(printf '%s\n' "$b") >&2 | head -40
    fi
}

# List surviving files under a dir, relative + sorted — for the orphan-decision
# comparison.
survivors() {
    local dir="$1"
    ( cd "$dir" && find . -type f -not -path '*/.*' | sed 's|^\./||' | LC_ALL=C sort )
}

# ---------------------------------------------------------------------------
# Build a fresh fixture: a synthetic output tree + a matching input digest.
# Returns via globals FIX_IN / FIX_OUT / FIX_DIG_BASH / FIX_DIG_ORACLE.
make_fixture() {
    local name="$1"
    local root="$TMP/$name"
    rm -rf "$root"; mkdir -p "$root/out/pkg/sub" "$root/build"
    echo "alpha content"  > "$root/out/pkg/Alpha.txt"
    echo "beta content"   > "$root/out/pkg/sub/Beta.txt"
    echo "gamma content"  > "$root/out/pkg/Gamma.txt"

    # Input digest: canonical typed InputDigest (#512). Hashes are arbitrary but
    # fixed — fresh compares recorded==current, both derived from THIS file, so
    # the values only need to be internally consistent across the two tools.
    cat > "$root/input.json" <<'EOF'
{
  "digestFormatVersion": 2,
  "moduleFormatVersion": 1,
  "selfHash": "selfhash-abc123",
  "dependencyHashes": [
    {"key": "hydra-kernel", "value": "dephash-kernel-999"}
  ],
  "moduleHashes": [
    {"key": "hydra.pkg.alpha", "value": "modhash-alpha-111"},
    {"key": "hydra.pkg.beta",  "value": "modhash-beta-222"}
  ]
}
EOF
    FIX_IN="$root/input.json"
    FIX_OUT="$root/out"
    FIX_DIG_BASH="$root/build/digest-bash.json"
    FIX_DIG_ORACLE="$root/build/digest-oracle.json"
}

run_bash() {   bash "$DIGEST_SH" "$@"; }

# ===========================================================================
echo "=== test-digest-conformance.sh (#416) ==="
if [ "$ORACLE" -eq 0 ]; then
    note "oracle OFF (set HYDRA_DIGEST_ORACLE=1 with a stack slot for full verification) — running bin/digest.sh self-consistency + verdict checks."
fi

# ---------------------------------------------------------------------------
# CASE 1: refresh — both tools record the same digest model over the same tree.
make_fixture case1
checks=$((checks+1))
run_bash refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null
# Self-consistency: bash refresh then bash fresh must HIT.
if run_bash fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null; then
    note "case1 self: bash refresh -> bash fresh HIT (ok)"
else
    report_fail "case1 self: bash refresh -> bash fresh unexpectedly MISSED"
fi
if [ "$ORACLE" -eq 1 ]; then
    oracle refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_ORACLE" >/dev/null
    assert_digests_equiv "case1 refresh" "$FIX_DIG_BASH" "$FIX_DIG_ORACLE"
    # Cross-read: oracle-written digest must be a HIT under bash fresh, and
    # bash-written digest must be a HIT under oracle fresh (transition safety).
    if ! run_bash fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_ORACLE" >/dev/null; then
        report_fail "case1 cross: bash fresh MISSED on oracle-written digest"
    fi
    if ! oracle fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null; then
        report_fail "case1 cross: oracle fresh MISSED on bash-written digest"
    fi
fi

# ---------------------------------------------------------------------------
# CASE 2: fresh verdict matrix — hit, generator-mismatch, output-tamper.
# For each, assert bash and oracle agree on the exit code (hit=0 / miss=1).
verdict_agree() {
    # $1 label; runs a bash and (if available) oracle fresh with the SAME args
    # ($2.. ) and asserts exit codes match. Returns the bash verdict via $VERDICT.
    local label="$1"; shift
    local b_rc o_rc
    set +e
    run_bash fresh "$@" >/dev/null 2>&1; b_rc=$?
    set -e
    VERDICT=$b_rc
    if [ "$ORACLE" -eq 1 ]; then
        set +e
        oracle fresh "$@" >/dev/null 2>&1; o_rc=$?
        set -e
        if [ "$b_rc" -ne "$o_rc" ]; then
            report_fail "$label: verdict differs (bash exit=$b_rc, oracle exit=$o_rc)"
        fi
    fi
}

make_fixture case2
checks=$((checks+1))
run_bash refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null
[ "$ORACLE" -eq 1 ] && oracle refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_ORACLE" >/dev/null

# 2a: clean hit
verdict_agree "case2a hit(bash-digest)" --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH"
[ "$VERDICT" -eq 0 ] || report_fail "case2a: expected HIT, bash exit=$VERDICT"

# 2b: generator stamp mismatch => miss
( export HYDRA_GENERATOR_STAMP="a-different-stamp"
  set +e
  bash "$DIGEST_SH" fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null 2>&1
  b_rc=$?
  o_rc=0
  if [ "$ORACLE" -eq 1 ]; then
      ( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack exec digest-check -- fresh \
          --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" ) >/dev/null 2>&1
      o_rc=$?
  fi
  [ "$b_rc" -eq 1 ] || { echo "DRIFT: case2b: generator-mismatch expected MISS, bash exit=$b_rc" >&2; exit 3; }
  if [ "$ORACLE" -eq 1 ] && [ "$o_rc" -ne 1 ]; then
      echo "DRIFT: case2b: generator-mismatch oracle exit=$o_rc (expected 1)" >&2; exit 3
  fi
) || fail=1

# 2c: output tamper => miss
echo "tampered" >> "$FIX_OUT/pkg/Alpha.txt"
verdict_agree "case2c tamper" --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH"
[ "$VERDICT" -eq 1 ] || report_fail "case2c: expected MISS after tamper, bash exit=$VERDICT"

# ---------------------------------------------------------------------------
# CASE 3: #393 orphan reconcile — both tools delete the same orphans, keep the
# same files, and end with an equivalent refreshed digest + a HIT verdict.
make_fixture case3
checks=$((checks+1))
# Record clean digests with both tools over the pristine tree.
run_bash refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT.bash" --output-digest "$FIX_DIG_BASH" >/dev/null 2>&1 || true
# Two independent copies of the tree so each tool reconciles its own.
cp -r "$FIX_OUT" "$FIX_OUT.bash"
cp -r "$FIX_OUT" "$FIX_OUT.oracle"
run_bash   refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT.bash"   --output-digest "$FIX_DIG_BASH"   >/dev/null
[ "$ORACLE" -eq 1 ] && oracle refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT.oracle" --output-digest "$FIX_DIG_ORACLE" >/dev/null

# Introduce identical orphans (extra file + extra dir) into both trees.
for t in bash oracle; do
    [ "$t" = "oracle" ] && [ "$ORACLE" -eq 0 ] && continue
    mkdir -p "$FIX_OUT.$t/pkg/orphandir"
    echo "orphan file"   > "$FIX_OUT.$t/pkg/Orphan.txt"
    echo "orphan nested" > "$FIX_OUT.$t/pkg/orphandir/Nested.txt"
done

if run_bash fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT.bash" --output-digest "$FIX_DIG_BASH" >/dev/null; then
    note "case3: bash reconcile reported HIT (ok)"
else
    report_fail "case3: bash fresh returned MISS during orphan reconcile (expected HIT)"
fi
# Bash must have deleted the orphans + pruned the empty dir.
if [ -e "$FIX_OUT.bash/pkg/Orphan.txt" ] || [ -e "$FIX_OUT.bash/pkg/orphandir" ]; then
    report_fail "case3: bash left orphan(s) behind after reconcile"
fi

if [ "$ORACLE" -eq 1 ]; then
    oracle fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT.oracle" --output-digest "$FIX_DIG_ORACLE" >/dev/null
    # Same survivors on both sides.
    if [ "$(survivors "$FIX_OUT.bash")" != "$(survivors "$FIX_OUT.oracle")" ]; then
        report_fail "case3: orphan-delete decision differs (bash vs oracle survivors)"
        diff <(survivors "$FIX_OUT.bash") <(survivors "$FIX_OUT.oracle") >&2 | head -20
    fi
    # Refreshed digests equivalent.
    assert_digests_equiv "case3 post-reconcile" "$FIX_DIG_BASH" "$FIX_DIG_ORACLE"
fi

# ---------------------------------------------------------------------------
# CASE 4 (#719 edit-race datapoint): the digest is a point-in-time snapshot of
# OUTPUT bytes. After a clean refresh+hit, if an output file changes, the NEXT
# fresh must MISS (both tools re-hash at invocation time and agree). And the
# manual escape hatch — deleting the output digest — must force a MISS.
make_fixture case4
checks=$((checks+1))
run_bash refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null
verdict_agree "case4 pre-edit hit" --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH"
[ "$VERDICT" -eq 0 ] || report_fail "case4: expected HIT before edit, bash exit=$VERDICT"
# Simulate the #719 race: an output changes AFTER the snapshot.
echo "post-snapshot edit" >> "$FIX_OUT/pkg/Gamma.txt"
verdict_agree "case4 post-edit miss" --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH"
[ "$VERDICT" -eq 1 ] || report_fail "case4: expected MISS after post-snapshot edit, bash exit=$VERDICT"
# Escape hatch: rm the output digest => MISS (unreadable => cache miss).
run_bash refresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null
rm -f "$FIX_DIG_BASH"
set +e
run_bash fresh --inputs "$FIX_IN" --output-dir "$FIX_OUT" --output-digest "$FIX_DIG_BASH" >/dev/null 2>&1
er=$?
set -e
[ "$er" -eq 1 ] || report_fail "case4: deleting output digest should force MISS, bash exit=$er"

# ---------------------------------------------------------------------------
# CASE 5 (refresh-input, #416): bin/digest.sh refresh-input ≡ digest-check
# refresh-input across the REAL call set (hydra-jvm/java/python/scala). Runs
# against a scratch copy of dist/json so the real digests aren't mutated. Both
# tools read the same source tree ($HYDRA_ROOT_DIR/packages) + the same scratch
# dist-json-root, so a hit compares apples to apples. Self-consistency (always):
# the bash-written digest's selfHash recomputes from its own moduleHashes, and
# the src-namespace key set is non-degenerate for the packages that own sources.
# Oracle (opt-in): full semantic equivalence of the written InputDigest.
REFRESH_INPUT_PKGS="hydra-jvm hydra-java hydra-python hydra-scala"
ri_root="$TMP/ri/dist-json"
for p in $REFRESH_INPUT_PKGS; do
    src="$HYDRA_ROOT_DIR/dist/json/$p"
    [ -d "$src/src/main/json" ] || continue   # skip cleanly if dist not synced
    mkdir -p "$ri_root/$p/src/main" "$ri_root/$p/build/main"
    cp -r "$src/src/main/json" "$ri_root/$p/src/main/json"
    [ -f "$src/build/main/digest.json" ] && cp "$src/build/main/digest.json" "$ri_root/$p/build/main/digest.json"
done
if [ -d "$ri_root" ]; then
    for p in $REFRESH_INPUT_PKGS; do
        [ -d "$ri_root/$p/src/main/json" ] || continue
        checks=$((checks+1))
        bash_dig="$ri_root/$p/build/main/digest.json"
        run_bash refresh-input --package "$p" --dist-json-root "$ri_root" >/dev/null

        # Self-consistency: selfHash recomputes from the written moduleHashes.
        # Mirror computeSelfHash exactly: sorted "ns\thash\n" lines, concatenated
        # (jq's -j join produces the exact byte stream, no trailing newline added).
        # Guarded (set +e around the pipe) so a jq/hash hiccup reports as a drift,
        # never aborts the harness via set -e/pipefail.
        set +e
        recomputed="$(jq -j '.moduleHashes | sort_by(.key) | map(.key + "\t" + .value + "\n") | join("")' "$bash_dig" 2>/dev/null | sha256sum | cut -d' ' -f1)"
        recorded_self="$(jq -r '.selfHash // ""' "$bash_dig" 2>/dev/null)"
        set -e
        if [ -n "$recorded_self" ] && [ "$recomputed" != "$recorded_self" ]; then
            report_fail "case5 $p: written selfHash does not recompute from moduleHashes"
        fi

        if [ "$ORACLE" -eq 1 ]; then
            # Oracle writes into a SEPARATE scratch root (same source tree) so the
            # two don't overwrite each other; compare the resulting digests.
            or_root="$TMP/ri-oracle/dist-json"
            mkdir -p "$or_root/$p/src/main" "$or_root/$p/build/main"
            cp -r "$HYDRA_ROOT_DIR/dist/json/$p/src/main/json" "$or_root/$p/src/main/json"
            [ -f "$HYDRA_ROOT_DIR/dist/json/$p/build/main/digest.json" ] && \
                cp "$HYDRA_ROOT_DIR/dist/json/$p/build/main/digest.json" "$or_root/$p/build/main/digest.json"
            oracle refresh-input --package "$p" --dist-json-root "$or_root" >/dev/null
            assert_digests_equiv "case5 refresh-input $p" "$bash_dig" "$or_root/$p/build/main/digest.json"
        fi
    done
else
    note "case5 refresh-input: dist/json not synced — skipped (no packages to check)."
fi

# ---------------------------------------------------------------------------
if [ "$checks" -eq 0 ]; then
    echo "test-digest-conformance: no cases ran — harness bug." >&2
    exit 1
fi

if [ "$fail" -ne 0 ]; then
    echo "test-digest-conformance: FAILED — bin/digest.sh drifted from the digest-check oracle." >&2
    exit 1
fi

if [ "$ORACLE" -eq 1 ]; then
    echo "test-digest-conformance: OK — bin/digest.sh matches digest-check across $checks case group(s) (oracle-verified)."
else
    echo "test-digest-conformance: OK (self-consistency; oracle SKIPPED — build digest-check for full verification) — $checks case group(s)."
fi
