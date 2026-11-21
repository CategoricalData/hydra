#!/usr/bin/env bash
# #831 regression: the per-target generator stamp must change when (1) the coder's authoring source
# changes and (2) the generator host's compiled copy of that coder changes — so a coder edit
# followed by a copy refresh invalidates the dependent target's digest without manual deletion.
# Hermetic: works on a temp copy of the minimal tree; stack-free.
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REAL_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
fail() { echo "FAIL: $*" >&2; exit 1; }
pass() { echo "ok: $*"; }

T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
# Pin python to the local-source path: this test is about the local coder fingerprint, and a
# published python host (no hostOverrides entry) fingerprints by version, ignoring coder.py edits.
jq '.hostOverrides.python = "local"' "$REAL_ROOT/hydra.json" > "$T/hydra.json"
mkdir -p "$T/bin/lib" "$T/heads" "$T/packages"
cp "$REAL_ROOT"/bin/lib/*.py "$REAL_ROOT"/bin/lib/*.sh "$T/bin/lib/"
cp -r "$REAL_ROOT/heads/python" "$REAL_ROOT/heads/haskell" "$REAL_ROOT/heads/java" "$T/heads/" 2>/dev/null || true
for p in hydra-kernel hydra-python; do
    mkdir -p "$T/packages/$p/src/main"
    cp -r "$REAL_ROOT/packages/$p/src/main/." "$T/packages/$p/src/main/"
    cp "$REAL_ROOT/packages/$p/package.json" "$T/packages/$p/"
done
gen_host="$(jq -r .generators.default "$T/hydra.json")"
mkdir -p "$T/dist/$gen_host/hydra-python/src/main/java"
echo "class Coder {}" > "$T/dist/$gen_host/hydra-python/src/main/java/Coder.java"

stamp() { ( export HYDRA_ROOT_DIR="$T"; source "$T/bin/lib/common.sh" 2>/dev/null || true
            source "$T/bin/lib/assemble-common.sh"; compute_generator_stamp python ); }

s0="$(stamp)"
[ "$s0" = "$(stamp)" ] || fail "stamp not deterministic"

echo "# edit" >> "$(find "$T/packages/hydra-python/src/main/python" -name coder.py | head -1)"
s1="$(stamp)"
[ "$s0" != "$s1" ] || fail "coder.py edit did not change the python stamp"
pass "coder source edit changes the stamp"

echo "class Coder { int x; }" > "$T/dist/$gen_host/hydra-python/src/main/java/Coder.java"
s2="$(stamp)"
[ "$s1" != "$s2" ] || fail "refreshing the generator host's coder copy did not change the python stamp (#831)"
pass "generator-host coder copy refresh changes the stamp"
