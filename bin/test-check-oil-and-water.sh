#!/usr/bin/env bash
# Regression test for #608's oil-and-water check (bin/check-oil-and-water.py):
# a driver's package.yaml must not both depend on a published hydra-<pkg> AND
# source-dir that package's HEAD authoring tree (packages/<pkg>/src/main/haskell).
#
# Builds a small SYNTHETIC fixture under a temp dir rather than depending on
# any real driver's current package.yaml shape.
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

make_fixture_repo() {
    local dir="$1"
    mkdir -p "$dir/heads/fixture-driver"
}

run_check() {
    local dir="$1"
    python3 "$CHECK" --root "$dir" 2>&1
}

echo "=== #608 fixture: published dep + HEAD authoring source-dir (violation) ==="
FIXTURE_A="$(mktemp -d)"
trap 'rm -rf "$FIXTURE_A"' EXIT
make_fixture_repo "$FIXTURE_A"
cat > "$FIXTURE_A/heads/fixture-driver/package.yaml" <<'EOF'
name: fixture-driver
dependencies:
  - base
  - hydra-kernel
library:
  source-dirs:
    - src/main/haskell
    - ../../packages/hydra-kernel/src/main/haskell
EOF

if OUT="$(run_check "$FIXTURE_A")"; then
    log_fail "violation-case: expected nonzero exit, got 0"
    echo "$OUT" | sed 's/^/    /'
else
    if echo "$OUT" | grep -q "oil-and-water violation"; then
        log_pass "violation-case: correctly flagged"
    else
        log_fail "violation-case: nonzero exit but wrong message"
        echo "$OUT" | sed 's/^/    /'
    fi
fi
rm -rf "$FIXTURE_A"
trap - EXIT

echo ""
echo "=== #608 fixture: published dep, no HEAD source-dir (OK) ==="
FIXTURE_B="$(mktemp -d)"
trap 'rm -rf "$FIXTURE_B"' EXIT
make_fixture_repo "$FIXTURE_B"
cat > "$FIXTURE_B/heads/fixture-driver/package.yaml" <<'EOF'
name: fixture-driver
dependencies:
  - base
  - hydra-kernel
library:
  source-dirs:
    - src/main/haskell
EOF

if OUT="$(run_check "$FIXTURE_B")"; then
    log_pass "published-only-case: passes"
else
    log_fail "published-only-case: expected zero exit"
    echo "$OUT" | sed 's/^/    /'
fi
rm -rf "$FIXTURE_B"
trap - EXIT

echo ""
echo "=== #608 fixture: HEAD source-dir, no published dep (OK — local-host build) ==="
FIXTURE_C="$(mktemp -d)"
trap 'rm -rf "$FIXTURE_C"' EXIT
make_fixture_repo "$FIXTURE_C"
cat > "$FIXTURE_C/heads/fixture-driver/package.yaml" <<'EOF'
name: fixture-driver
dependencies:
  - base
library:
  source-dirs:
    - src/main/haskell
    - ../../packages/hydra-kernel/src/main/haskell
EOF

if OUT="$(run_check "$FIXTURE_C")"; then
    log_pass "local-host-case: passes"
else
    log_fail "local-host-case: expected zero exit"
    echo "$OUT" | sed 's/^/    /'
fi
rm -rf "$FIXTURE_C"
trap - EXIT

echo ""
echo "=== sanity: real repo (heads/) passes clean ==="
if OUT="$(python3 "$CHECK" --root "$HYDRA_ROOT_DIR" 2>&1)"; then
    log_pass "real repo passes"
else
    log_fail "real repo unexpectedly failed"
    echo "$OUT" | sed 's/^/    /'
fi

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
if [ "$FAIL" -ne 0 ]; then
    exit 1
fi
