#!/usr/bin/env bash
# Regression test for #617's Check 2 (unpublished Hydra.Build.* import in the
# cold-seeder's closure), added to bin/check-oil-and-water.py.
#
# Builds a small SYNTHETIC git repo fixture under a temp dir (its own commits,
# its own merge-base) rather than repro'ing today's live Generation.hs shim
# state — the fixture must stay valid regardless of when staging lands the
# real structural fix for #607, and must not require a Haskell toolchain.
#
# Two independent scenarios, each with its own before/after commit pair:
#   (a) module-absent:  Hydra.Build.<Mod> imported, and <Mod> is not in the
#                        hydra-build manifest at all at merge-base.
#   (b) import-new:      Hydra.Build.<Mod> imported, <Mod> DOES exist in the
#                        hydra-build manifest at merge-base, but this
#                        particular import is new-on-branch (the #607 shape:
#                        ManifestWriter predates the branch; the import into
#                        Generation.hs does not).
# For each scenario: FAIL when the offending import is present, PASS once it
# is removed.
#
# Usage:
#   bin/test-check-oil-and-water.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
CHECK="$SCRIPT_DIR/check-oil-and-water.py"

PASS=0
FAIL=0

log_pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
log_fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

# Build a minimal fixture repo: just enough for check-oil-and-water.py to
# find heads/**/package.yaml (Check 1, empty-but-present so main() doesn't
# bail) and the Check-2 closure roots + tracked hydra-build manifest.
make_fixture_repo() {
    local dir="$1"
    mkdir -p "$dir"
    ( cd "$dir" && git init -q && git config user.email "test@example.com" && git config user.name "test" )

    mkdir -p "$dir/heads/haskell/json-driver/app"
    mkdir -p "$dir/heads/haskell/src/main/haskell/Hydra"
    mkdir -p "$dir/dist/json/hydra-build/src/main/json"

    # Minimal package.yaml so Check 1's glob finds something (no violation:
    # no source-dirs / dependencies blocks at all).
    cat > "$dir/heads/haskell/json-driver/package.yaml" <<'EOF'
name: fixture-json-driver
EOF

    # The four headmods stand-ins. Only Generation.hs carries content that
    # varies per scenario; the others are present-but-inert so the closure
    # walk has real files to read.
    cat > "$dir/heads/haskell/json-driver/app/ColdSeedMain.hs" <<'EOF'
module Main where
import Hydra.Kernel
main :: IO ()
main = return ()
EOF
    for m in PackageRouting TargetFilePaths Digest; do
        cat > "$dir/heads/haskell/src/main/haskell/Hydra/$m.hs" <<EOF
module Hydra.$m where
import Hydra.Kernel
EOF
    done
}

write_manifest() {
    local dir="$1"; shift
    local modules=("$@")
    python3 - "$dir/dist/json/hydra-build/src/main/json/manifest.json" "${modules[@]}" <<'EOF'
import json, sys
path = sys.argv[1]
mods = sys.argv[2:]
json.dump({"package": "hydra-build", "mainModules": mods, "testModules": []}, open(path, "w"))
EOF
}

commit_all() {
    local dir="$1"; local msg="$2"
    ( cd "$dir" && git add -A && git commit -q -m "$msg" )
}

run_check() {
    local dir="$1"
    python3 "$CHECK" --root "$dir" --merge-base-ref "HEAD~1" 2>&1
}

echo "=== #617 fixture (a): module-absent at merge-base ==="
FIXTURE_A="$(mktemp -d)"
trap 'rm -rf "$FIXTURE_A"' EXIT
make_fixture_repo "$FIXTURE_A"
write_manifest "$FIXTURE_A" "hydra.build.modules" "hydra.build.routing"
commit_all "$FIXTURE_A" "merge-base: hydra-build without ManifestWriter"

cat > "$FIXTURE_A/heads/haskell/src/main/haskell/Hydra/Generation.hs" <<'EOF'
module Hydra.Generation where
import Hydra.Kernel
import qualified Hydra.Build.ManifestWriter as GenManifestWriter
EOF
write_manifest "$FIXTURE_A" "hydra.build.modules" "hydra.build.routing" "hydra.build.manifestWriter"
commit_all "$FIXTURE_A" "HEAD: add ManifestWriter import (module also newly published)"

if OUT="$(run_check "$FIXTURE_A")"; then
    log_fail "(a)-before: expected nonzero exit (module-absent-at-merge-base violation), got 0"
    echo "$OUT" | sed 's/^/    /'
else
    if echo "$OUT" | grep -q "not in hydra-build's published module set"; then
        log_pass "(a)-before: correctly flagged module-absent-at-merge-base"
    else
        log_fail "(a)-before: nonzero exit but wrong message"
        echo "$OUT" | sed 's/^/    /'
    fi
fi

# "Fix": drop the import (mirrors the real shim's structural fix direction).
cat > "$FIXTURE_A/heads/haskell/src/main/haskell/Hydra/Generation.hs" <<'EOF'
module Hydra.Generation where
import Hydra.Kernel
EOF
commit_all "$FIXTURE_A" "fix: drop ManifestWriter import"
if OUT="$(run_check "$FIXTURE_A")"; then
    log_pass "(a)-after: passes once the import is removed"
else
    log_fail "(a)-after: expected zero exit after removing the import"
    echo "$OUT" | sed 's/^/    /'
fi
rm -rf "$FIXTURE_A"
trap - EXIT

echo ""
echo "=== #617 fixture (b): import-new, module predates merge-base (the #607 shape) ==="
FIXTURE_B="$(mktemp -d)"
trap 'rm -rf "$FIXTURE_B"' EXIT
make_fixture_repo "$FIXTURE_B"
# ManifestWriter already published at merge-base — but nothing imports it yet.
write_manifest "$FIXTURE_B" "hydra.build.modules" "hydra.build.routing" "hydra.build.manifestWriter"
cat > "$FIXTURE_B/heads/haskell/src/main/haskell/Hydra/Generation.hs" <<'EOF'
module Hydra.Generation where
import Hydra.Kernel
EOF
commit_all "$FIXTURE_B" "merge-base: hydra-build WITH ManifestWriter, no import yet"

# HEAD adds the import — module predates the branch, but this call site is new.
cat > "$FIXTURE_B/heads/haskell/src/main/haskell/Hydra/Generation.hs" <<'EOF'
module Hydra.Generation where
import Hydra.Kernel
import qualified Hydra.Build.ManifestWriter as GenManifestWriter
EOF
commit_all "$FIXTURE_B" "HEAD: add new call site importing the pre-existing module"

if OUT="$(run_check "$FIXTURE_B")"; then
    log_fail "(b)-before: expected nonzero exit (import-new-since-merge-base violation), got 0"
    echo "$OUT" | sed 's/^/    /'
else
    if echo "$OUT" | grep -q "newly imports Hydra.Build"; then
        log_pass "(b)-before: correctly flagged import-new-since-merge-base"
    else
        log_fail "(b)-before: nonzero exit but wrong message"
        echo "$OUT" | sed 's/^/    /'
    fi
fi

cat > "$FIXTURE_B/heads/haskell/src/main/haskell/Hydra/Generation.hs" <<'EOF'
module Hydra.Generation where
import Hydra.Kernel
EOF
commit_all "$FIXTURE_B" "fix: drop the new call site"
if OUT="$(run_check "$FIXTURE_B")"; then
    log_pass "(b)-after: passes once the new call site is removed"
else
    log_fail "(b)-after: expected zero exit after removing the import"
    echo "$OUT" | sed 's/^/    /'
fi
rm -rf "$FIXTURE_B"
trap - EXIT

echo ""
echo "=== #617 fixture (c): sanity — real repo (heads/haskell) passes clean ==="
if OUT="$(python3 "$CHECK" --root "$HYDRA_ROOT_DIR" 2>&1)"; then
    log_pass "(c): real repo passes with default --merge-base-ref origin/main"
else
    log_fail "(c): real repo unexpectedly failed"
    echo "$OUT" | sed 's/^/    /'
fi

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
if [ "$FAIL" -ne 0 ]; then
    exit 1
fi
