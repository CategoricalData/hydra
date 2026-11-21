#!/usr/bin/env bash
# Hermetic regression test (#672) for two invariants that keep ordinary syncs off the Haskell build:
#
#   F1  bin/lib/check-sources-fresh.py (the #773 fast-path gate in bin/sync.sh) reports a module-source
#       edit as stale, both on a cold tree (no stamp: compared against the merge-base with origin/main)
#       and on a warm tree (compared against the stamp recorded after Phase 1). A miss here means a
#       source edit followed by a non-Haskell sync silently leaves dist/json stale.
#   K10/K11  bin/test.sh and demos/bootstrapping/bin/bootstrap-all.sh take the shared-~/.stack build
#       slot only when haskell is in scope, and forward the original argv when they do.
#
# Uses a synthetic git repository and a fake with-stack-slot.sh under a temp dir; touches nothing in
# the worktree. Needs only bash, git and python3.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
FAILS=0
check() {  # check <description> <expected-rc> <command...>
    local desc="$1" want="$2"; shift 2
    local rc=0
    "$@" >/dev/null 2>&1 || rc=$?
    if [ "$rc" = "$want" ]; then echo "  ok    $desc"; else echo "  FAIL  $desc (rc=$rc, want $want)"; FAILS=$((FAILS + 1)); fi
}

echo "=== F1: check-sources-fresh.py ==="
REPO="$TMP/repo"
mkdir -p "$REPO/bin/lib" "$REPO/packages/hydra-kernel/src/main/hydra/hydra/core" \
         "$REPO/packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg" "$REPO/packages/hydra-pg/src/main/haskell/Other"
cp "$ROOT/bin/lib/check-sources-fresh.py" "$REPO/bin/lib/"
FRESH=("python3" "$REPO/bin/lib/check-sources-fresh.py" "$REPO")
echo "module hydra.core.a" > "$REPO/packages/hydra-kernel/src/main/hydra/hydra/core/a.hydra"
echo '{"name": "hydra-kernel"}' > "$REPO/packages/hydra-kernel/package.json"
echo "module Hydra.Sources.Pg.Model where" > "$REPO/packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg/Model.hs"
echo "module Other.Helper where" > "$REPO/packages/hydra-pg/src/main/haskell/Other/Helper.hs"
git -C "$REPO" init -q -b main
git -C "$REPO" -c user.email=t@t -c user.name=t add -A
git -C "$REPO" -c user.email=t@t -c user.name=t commit -q -m init
git -C "$REPO" update-ref refs/remotes/origin/main HEAD

check "cold: unmodified sources are fresh" 0 "${FRESH[@]}"
echo "-- edit" >> "$REPO/packages/hydra-kernel/src/main/hydra/hydra/core/a.hydra"
check "cold: modified .hydra is stale" 1 "${FRESH[@]}"
git -C "$REPO" checkout -q -- packages
echo "module hydra.core.b" > "$REPO/packages/hydra-kernel/src/main/hydra/hydra/core/b.hydra"
check "cold: untracked .hydra is stale" 1 "${FRESH[@]}"
rm "$REPO/packages/hydra-kernel/src/main/hydra/hydra/core/b.hydra"
rm "$REPO/packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg/Model.hs"
check "cold: deleted Haskell DSL source is stale" 1 "${FRESH[@]}"
git -C "$REPO" checkout -q -- packages
echo "-- edit" >> "$REPO/packages/hydra-pg/src/main/haskell/Other/Helper.hs"
check "cold: non-source Haskell file does not count" 0 "${FRESH[@]}"
git -C "$REPO" checkout -q -- packages
echo "-- edit" >> "$REPO/packages/hydra-kernel/src/main/hydra/hydra/core/a.hydra"
git -C "$REPO" -c user.email=t@t -c user.name=t commit -qam "branch edit"
check "cold: source edit committed on the branch is stale" 1 "${FRESH[@]}"
"${FRESH[@]}" --record
check "warm: stamp recorded after Phase 1 is fresh" 0 "${FRESH[@]}"
echo '{"name": "hydra-kernel", "textualSources": "all"}' > "$REPO/packages/hydra-kernel/package.json"
check "warm: package.json edit after the stamp is stale" 1 "${FRESH[@]}"

echo "=== K10/K11: build-slot guards ==="
T="$TMP/slot"
mkdir -p "$T/bin" "$T/demos/bootstrapping/bin" "$T/dist/json/hydra-build/src/main/json"
cp "$ROOT/bin/test.sh" "$T/bin/"
cp "$ROOT/demos/bootstrapping/bin/bootstrap-all.sh" "$T/demos/bootstrapping/bin/"
echo '{"testMatrix": ["haskell", "java", "python"], "lisp": []}' > "$T/dist/json/hydra-build/src/main/json/languages.json"
# The fake slot wrapper records its argv and stops; the fake sync.sh stops the real work.
printf '#!/bin/bash\necho "$@" > "%s/slot.log"\nexit 0\n' "$T" > "$T/bin/with-stack-slot.sh"
printf '#!/bin/bash\nexit 0\n' > "$T/bin/sync.sh"
chmod +x "$T/bin/"*.sh "$T/demos/bootstrapping/bin/"*.sh
slot_taken() {  # slot_taken <expected yes|no> <command...>; also checks argv is forwarded
    local want="$1"; shift
    rm -f "$T/slot.log"
    (cd "$T" && env -u HYDRA_STACK_SLOT_HELD timeout 20 "$@" >/dev/null 2>&1) || true
    local got=no
    if [ -f "$T/slot.log" ]; then
        got=yes
        local fwd="${*:2}"
        if [[ "$(cat "$T/slot.log")" != *"-- $1 $fwd" ]]; then
            echo "  FAIL  $* : slot taken but argv not forwarded ($(cat "$T/slot.log"))"; FAILS=$((FAILS + 1)); return
        fi
    fi
    if [ "$got" = "$want" ]; then echo "  ok    slot=$want: $*"; else echo "  FAIL  slot=$got (want $want): $*"; FAILS=$((FAILS + 1)); fi
}
slot_taken no  bin/test.sh --no-sync java,python
slot_taken yes bin/test.sh --no-sync haskell,java
slot_taken yes bin/test.sh --no-sync all
slot_taken no  demos/bootstrapping/bin/bootstrap-all.sh --hosts java --targets python
slot_taken yes demos/bootstrapping/bin/bootstrap-all.sh --hosts java --targets haskell,python --tag x
slot_taken yes demos/bootstrapping/bin/bootstrap-all.sh --hosts all
slot_taken no  demos/bootstrapping/bin/bootstrap-all.sh test --paths java-to-python
slot_taken yes demos/bootstrapping/bin/bootstrap-all.sh test
slot_taken no  demos/bootstrapping/bin/bootstrap-all.sh display

echo ""
if [ "$FAILS" -gt 0 ]; then echo "test-sources-freshness: $FAILS failure(s)"; exit 1; fi
echo "test-sources-freshness: all checks passed"
