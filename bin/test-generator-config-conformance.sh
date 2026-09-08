#!/usr/bin/env bash
# #559 conformance guard: the generator/host set is CONSUMER CONFIG, external to published
# hydra-build — not a `case host in java|haskell` and not a baked-in host table in the build
# logic. This test fails if that neutrality regresses:
#
#   1. run_layer1_transform / driver_identity dispatch generically over the hydra.json
#      "generators" section — they must NOT re-introduce a literal host-name branch
#      (`case ... in java)`, `if ... = "haskell"`) in the dispatch logic.
#   2. The dispatch resolves a host's transform binary + driver sources from config, so a host
#      declared ONLY in config (no script edit) is dispatchable, and an undeclared host fails loud.
#   3. Adding a host is a config edit: a temp config with a synthetic host routes to that host's
#      declared binary with ZERO edit to bin/lib/assemble-common.sh.
#
# Hermetic, stack-free, fast. Wired into test-regressions.sh. (#559 Phase-A host-independence,
# external-consumer dimension; Design-D: pure-bash driver + this conformance oracle.)
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"
export HYDRA_ROOT_DIR
cd "$HYDRA_ROOT_DIR"

fail() { echo "FAIL: $*" >&2; exit 1; }
pass() { echo "ok: $*"; }

# shellcheck source=/dev/null
source "$HYDRA_ROOT_DIR/bin/lib/common.sh" 2>/dev/null || true
# shellcheck source=/dev/null
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# --- 1. no literal host-name branch in the dispatch logic ---------------------
# The dispatch functions must fold over config, not branch on host literals. Flag a
# `case`/`if` on the effective-host that hardcodes java|haskell in run_layer1_transform.
dispatch_body="$(awk '/^run_layer1_transform\(\)/,/^}/' bin/lib/assemble-common.sh)"
if printf '%s\n' "$dispatch_body" | grep -Eq '(case[[:space:]]+"?\$(effective_host|GENERATOR_HOST)"?[[:space:]]+in|=[[:space:]]*"(java|haskell)")'; then
    fail "run_layer1_transform re-introduced a literal host-name branch (java|haskell) in dispatch logic"
fi
pass "run_layer1_transform has no literal host-name branch"

# --- 2. config resolution + fail-loud -----------------------------------------
default_host="$(generator_config '.default')"
[ -n "$default_host" ] || fail "generators.default missing from hydra.json"
[ -n "$(generator_config ".hosts[\"$default_host\"].transform")" ] \
    || fail "generators.hosts.$default_host.transform missing"
pass "default host '$default_host' resolves a transform binary"

# driver_identity resolves + hashes for the default host
di_default="$(driver_identity)"
[ -n "$di_default" ] || fail "driver_identity empty for default host"
# a different declared host yields a different driver fingerprint (Bug C: tracks ACTIVE driver)
other_host="$(jq -r '.generators.hosts | keys[]' hydra.json | grep -v "^$default_host\$" | head -1)"
if [ -n "$other_host" ]; then
    di_other="$(GENERATOR_HOST="$other_host" driver_identity)"
    [ "$di_default" != "$di_other" ] \
        || fail "driver_identity identical for '$default_host' and '$other_host' — not tracking active driver (Bug C)"
    pass "driver_identity tracks the active generator host ($default_host != $other_host)"
fi

# unknown host fails loud (non-zero) in both dispatch and stamp
if GENERATOR_HOST=__no_such_host__ driver_identity >/dev/null 2>&1; then
    fail "driver_identity did not fail loud on an unknown host"
fi
pass "driver_identity fails loud on an unknown host"

# --- 3. add-a-host-by-config-only ---------------------------------------------
# A synthetic host declared ONLY in a temp config must be dispatchable with no script edit.
tmp_root="$(mktemp -d)"
trap 'rm -rf "$tmp_root"' EXIT
mkdir -p "$tmp_root/heads/synthetic/bin"
cat > "$tmp_root/heads/synthetic/bin/transform-json-to-target.sh" <<'STUB'
#!/usr/bin/env bash
echo "SYNTHETIC_GENERATOR_INVOKED $*"
STUB
chmod +x "$tmp_root/heads/synthetic/bin/transform-json-to-target.sh"
jq '.generators = {"default":"synthetic","hosts":{"synthetic":{"transform":"heads/synthetic/bin/transform-json-to-target.sh","driverSources":["heads/synthetic/bin/transform-json-to-target.sh"]}}}' \
    hydra.json > "$tmp_root/hydra.json"
out="$(cd "$tmp_root" && HYDRA_ROOT_DIR="$tmp_root" bash -c 'source "'"$HYDRA_ROOT_DIR"'/bin/lib/assemble-common.sh"; run_layer1_transform sometarget somepkg' 2>&1)" || fail "synthetic host dispatch failed: $out"
printf '%s\n' "$out" | grep -q 'SYNTHETIC_GENERATOR_INVOKED sometarget somepkg' \
    || fail "config-only host was not dispatched to its declared binary; got: $out"
pass "a host declared only in config is dispatchable with zero script edit"

echo "PASS: generator-config conformance (#559 host-set is external consumer config)"
