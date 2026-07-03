#!/usr/bin/env bash
# Regression test for #540 — generate-head-haskell-build.py header double-prepend.
#
# Runs the generator twice in a row, in both --mode published and --mode
# local, and asserts the emitted heads/haskell/{package.yaml,stack.yaml}
# are byte-identical between the two runs (idempotency) and each contains
# exactly one copy of the generated-file header.
#
# Also plants a wording-drifted header-like line (simulating an older/newer
# script version's header surviving in HEAD) and asserts the generator fails
# loud rather than silently re-stacking a second header on top of it.
#
# Restores the pre-test working-tree contents of the two files on exit,
# whether the test passes or fails.
#
# Usage:
#   bin/test-header-idempotency.sh

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
GENERATOR="$HYDRA_ROOT_DIR/bin/lib/generate-head-haskell-build.py"
PKG_YAML="$HYDRA_ROOT_DIR/heads/haskell/package.yaml"
STACK_YAML="$HYDRA_ROOT_DIR/heads/haskell/stack.yaml"

PASS=0
FAIL=0

BACKUP_DIR="$(mktemp -d -t test-header-idempotency.XXXXXX)"
cp "$PKG_YAML" "$BACKUP_DIR/package.yaml.orig"
cp "$STACK_YAML" "$BACKUP_DIR/stack.yaml.orig"
restore() {
    cp "$BACKUP_DIR/package.yaml.orig" "$PKG_YAML"
    cp "$BACKUP_DIR/stack.yaml.orig" "$STACK_YAML"
    rm -rf "$BACKUP_DIR"
}
trap restore EXIT

check_mode() {
    local mode="$1"

    python3 "$GENERATOR" --mode "$mode" >/dev/null
    cp "$PKG_YAML" "$BACKUP_DIR/package.yaml.run1"
    cp "$STACK_YAML" "$BACKUP_DIR/stack.yaml.run1"

    python3 "$GENERATOR" --mode "$mode" >/dev/null

    if diff -q "$BACKUP_DIR/package.yaml.run1" "$PKG_YAML" >/dev/null; then
        echo "[$mode] PASS: package.yaml byte-identical across 2 runs"
        PASS=$((PASS + 1))
    else
        echo "[$mode] FAIL: package.yaml differs between run 1 and run 2"
        diff "$BACKUP_DIR/package.yaml.run1" "$PKG_YAML" || true
        FAIL=$((FAIL + 1))
    fi

    if diff -q "$BACKUP_DIR/stack.yaml.run1" "$STACK_YAML" >/dev/null; then
        echo "[$mode] PASS: stack.yaml byte-identical across 2 runs"
        PASS=$((PASS + 1))
    else
        echo "[$mode] FAIL: stack.yaml differs between run 1 and run 2"
        diff "$BACKUP_DIR/stack.yaml.run1" "$STACK_YAML" || true
        FAIL=$((FAIL + 1))
    fi

    local pkg_header_count stack_header_count expected_count
    pkg_header_count=$(grep -c "Note: this is an automatically generated" "$PKG_YAML" || true)
    stack_header_count=$(grep -c "Note: this is an automatically generated" "$STACK_YAML" || true)
    # local mode restores the hand-maintained form verbatim: no header at all.
    if [ "$mode" = "local" ]; then expected_count=0; else expected_count=1; fi

    if [ "$pkg_header_count" -eq "$expected_count" ]; then
        echo "[$mode] PASS: package.yaml has exactly $expected_count header block(s)"
        PASS=$((PASS + 1))
    else
        echo "[$mode] FAIL: package.yaml has $pkg_header_count header blocks (expected $expected_count)"
        FAIL=$((FAIL + 1))
    fi

    if [ "$stack_header_count" -eq "$expected_count" ]; then
        echo "[$mode] PASS: stack.yaml has exactly $expected_count header block(s)"
        PASS=$((PASS + 1))
    else
        echo "[$mode] FAIL: stack.yaml has $stack_header_count header blocks (expected $expected_count)"
        FAIL=$((FAIL + 1))
    fi
}

check_mode "published"
check_mode "local"

check_drift_guard() {
    # _strip_header() is the unit under test here, not the CLI: HEAD can't be
    # cheaply faked with a wording-drifted header without committing a real
    # fixture commit (which would pollute branch history), and
    # git_show_committed() always reads from HEAD, never the working tree.
    # Import the module and call _strip_header() directly with a drifted
    # header-like line.
    local log="$BACKUP_DIR/drift.log"
    if python3 - "$GENERATOR" >"$log" 2>&1 <<'EOF'
import importlib.util
import sys

spec = importlib.util.spec_from_file_location("gen", sys.argv[1])
gen = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gen)

drifted = "# Note: this is an automatically generated file (wording drift fixture).\nname: hydra\n"
gen._strip_header(drifted)
print("UNEXPECTED: _strip_header returned normally on drifted input")
sys.exit(0)
EOF
    then
        echo "[drift-guard] FAIL: _strip_header exited 0 on a wording-drifted header (should fail loud)"
        cat "$log"
        FAIL=$((FAIL + 1))
    elif grep -q "wording drift" "$log"; then
        echo "[drift-guard] PASS: _strip_header failed loud with a wording-drift message"
        PASS=$((PASS + 1))
    else
        echo "[drift-guard] FAIL: _strip_header exited nonzero but without the expected message"
        cat "$log"
        FAIL=$((FAIL + 1))
    fi
}

check_drift_guard

echo ""
echo "=== test-header-idempotency.sh: $PASS pass, $FAIL fail ==="

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
