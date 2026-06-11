#!/usr/bin/env bash
# Regression test for #469: a JSON content change in
# dist/json/<pkg>/src/main/json/ must invalidate the Phase-2
# (JSON -> Haskell render) freshness gate, so the next
# assemble-distribution.sh run regenerates the .hs.
#
# Construction:
#   1. Pick a native-coder package (hydra-python) where the JSON is
#      written by a runtime that the per-package source hashes cannot
#      see. This is the recurrence pattern from #398 / #469.
#   2. Run a clean assemble to seed both input and output digests.
#   3. Mutate a .json file under dist/json/<pkg>/.../ — semantically
#      equivalent (re-indented), so any re-render produces the same
#      bytes.
#   4. Run `digest-check refresh-input --package <pkg>` (the hook the
#      native drivers now invoke after writing JSON).
#   5. Re-run assemble-distribution.sh and assert the gate reports a
#      cache miss. The committed .hs should remain byte-identical
#      because the JSON change was semantically a no-op.
#
# Invariant tested: with #469's fix, a JSON content change in
# dist/json/<pkg>/ is observable to the freshness gate. Pre-#469, the
# gate hashed only sources and would skip the render even when the
# JSON-the-assembler-consumes had changed.
#
# Usage:
#   bin/test-json-content-invalidates-render.sh [hydra-java|hydra-python|all]

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

PACKAGES=("${1:-all}")
if [ "${PACKAGES[0]}" = "all" ]; then
    PACKAGES=(hydra-python hydra-java)
fi

PASS=0
FAIL=0

# Pick a .json under <pkg> to mutate. Coder JSON is the heaviest and
# the one #398 actually touched.
target_json_for() {
    case "$1" in
        hydra-python) echo "dist/json/hydra-python/src/main/json/hydra/python/coder.json" ;;
        hydra-java)   echo "dist/json/hydra-java/src/main/json/hydra/java/coder.json" ;;
        *)            echo "" ;;
    esac
}

# Mutate the file in place to a byte-different but semantically
# equivalent form (re-indented JSON). Python is the simplest tool that's
# universally available.
mutate_json() {
    local fp="$1"
    python3 -c "
import json, sys
with open('$fp') as f:
    data = json.load(f)
with open('$fp', 'w') as f:
    json.dump(data, f, indent=4)
" || return 1
}

# Build digest-check + bootstrap-from-json from current source.
# stack-exec runs whatever's in .stack-work; on a stale build cache the
# refresh-input subcommand (added by #469) may not exist yet. Mirror
# the test-stale-output-prune.sh pattern: build before exec.
echo "Building digest-check + bootstrap-from-json from current source..."
( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack build hydra:exe:digest-check hydra:exe:bootstrap-from-json )

run_one() {
    local pkg="$1"
    local rel_json
    rel_json="$(target_json_for "$pkg")"
    if [ -z "$rel_json" ] || [ ! -f "$HYDRA_ROOT_DIR/$rel_json" ]; then
        echo "[$pkg] SKIP: target json $rel_json missing (run sync first)"
        return 0
    fi
    local abs_json="$HYDRA_ROOT_DIR/$rel_json"
    local backup
    backup="$(mktemp -t test-469-$pkg-XXXXXX.json)"
    cp "$abs_json" "$backup"

    # Make sure both digests start fresh-and-valid: drop them and run a
    # clean assemble to seed them.
    rm -f "$HYDRA_ROOT_DIR/dist/json/$pkg/build/main/digest.json"
    rm -f "$HYDRA_ROOT_DIR/dist/haskell/$pkg/build/main/digest.json"

    echo "[$pkg] Setup: seeding input + output digests via assemble"
    ( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack exec digest-check -- \
        refresh-input --package "$pkg" \
        --dist-json-root "$HYDRA_ROOT_DIR/dist/json" ) >/dev/null
    if ! "$HYDRA_ROOT_DIR/heads/haskell/bin/assemble-distribution.sh" "$pkg" \
            >/tmp/test-469-seed-$pkg.log 2>&1; then
        echo "[$pkg] FAIL: seed assemble exit nonzero (log: /tmp/test-469-seed-$pkg.log)"
        tail -20 /tmp/test-469-seed-$pkg.log
        cp "$backup" "$abs_json"
        rm -f "$backup"
        FAIL=$((FAIL + 1))
        return
    fi

    # Snapshot the committed .hs hashes so we can verify they don't
    # actually change (the JSON mutation is semantically a no-op).
    local snapshot
    snapshot="$(mktemp -d -t test-469-$pkg-hs-XXXXXX)"
    cp -R "$HYDRA_ROOT_DIR/dist/haskell/$pkg/src/main/haskell" "$snapshot/"

    echo "[$pkg] Mutate: re-indenting $rel_json (semantic no-op, byte change)"
    mutate_json "$abs_json" || {
        echo "[$pkg] FAIL: python3 re-indent failed"
        cp "$backup" "$abs_json"
        rm -f "$backup"
        rm -rf "$snapshot"
        FAIL=$((FAIL + 1))
        return
    }

    # Sanity: bytes really did change.
    if cmp -s "$abs_json" "$backup"; then
        echo "[$pkg] FAIL: mutation was a no-op (re-indent produced identical bytes)"
        cp "$backup" "$abs_json"
        rm -f "$backup"
        rm -rf "$snapshot"
        FAIL=$((FAIL + 1))
        return
    fi

    # Refresh the input digest the way the native driver wrappers do.
    echo "[$pkg] Refresh input digest"
    ( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack exec digest-check -- \
        refresh-input --package "$pkg" \
        --dist-json-root "$HYDRA_ROOT_DIR/dist/json" ) >/dev/null

    # Re-run assemble. The gate must report a cache MISS — that is the
    # #469 invariant. Pre-fix, the per-package input digest would not
    # see the JSON content change and the gate would say "cache hit".
    local log=/tmp/test-469-postmutate-$pkg.log
    if ! "$HYDRA_ROOT_DIR/heads/haskell/bin/assemble-distribution.sh" "$pkg" \
            >"$log" 2>&1; then
        echo "[$pkg] FAIL: post-mutation assemble exit nonzero (log: $log)"
        tail -20 "$log"
        cp "$backup" "$abs_json"
        rm -f "$backup"
        rm -rf "$snapshot"
        FAIL=$((FAIL + 1))
        return
    fi

    if grep -q "cache hit" "$log"; then
        echo "[$pkg] FAIL: assemble reported cache HIT after JSON mutation; #469 not fixed."
        echo "[$pkg]   See $log"
        cp "$backup" "$abs_json"
        rm -f "$backup"
        rm -rf "$snapshot"
        FAIL=$((FAIL + 1))
        return
    fi

    # The .hs should be byte-identical to the snapshot (JSON change
    # was semantically a no-op).
    if ! diff -r "$snapshot/haskell" "$HYDRA_ROOT_DIR/dist/haskell/$pkg/src/main/haskell" >/dev/null; then
        echo "[$pkg] WARN: .hs content changed after re-render. Expected byte-identical."
        echo "[$pkg]       This is not a #469 failure but suggests the JSON re-indent"
        echo "[$pkg]       was not actually semantic-only; investigate."
    fi

    # Restore the committed JSON.
    cp "$backup" "$abs_json"
    rm -f "$backup"
    rm -rf "$snapshot"

    # Re-refresh the digest so we leave the tree in a consistent state.
    ( cd "$HYDRA_ROOT_DIR/heads/haskell" && stack exec digest-check -- \
        refresh-input --package "$pkg" \
        --dist-json-root "$HYDRA_ROOT_DIR/dist/json" ) >/dev/null

    echo "[$pkg] PASS"
    PASS=$((PASS + 1))
}

for p in "${PACKAGES[@]}"; do
    run_one "$p" || true
done

echo ""
echo "=== test-json-content-invalidates-render.sh: $PASS pass, $FAIL fail ==="

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
