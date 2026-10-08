#!/usr/bin/env bash
set -euo pipefail

# Convergence gate for docs/specification/{primitives,types}/*.md (#723 deliverable 4).
#
# Mirrors bin/prepare-release.sh's lexicon freshness gate (regenerate, diff against
# a backup, restore), but with a 3-state model instead of a binary pass/fail: most
# pages are NOT expected to match generated output yet (#723's own issue text:
# "Missing doc strings render as a visible marker, not a failure" -- kernel
# doc-string coverage is a separate, ongoing effort). The only failure this gate
# reports is a REGRESSION: a page listed in docs/specification/CONVERGED-PAGES.txt
# (meaning a prior session confirmed it matches and stripped its IOU header) that
# no longer matches generated output. Un-converged pages are reported but do not
# fail the gate.
#
# Usage:
#   ./bin/check-spec-freshness.sh
#
# Prerequisites: dist/json/hydra-kernel populated (bin/sync.sh or bin/sync-java.sh).
#
# To mark a page as converged (after manually verifying bin/regenerate-spec.sh's
# output for it matches, and stripping its IOU header per
# .claude/commands/regenerate-spec.md step 5): add its path, relative to
# docs/specification/, to docs/specification/CONVERGED-PAGES.txt.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
SPEC_DIR="$HYDRA_ROOT/docs/specification"
CONVERGED_LIST="$SPEC_DIR/CONVERGED-PAGES.txt"

if [ ! -d "$HYDRA_ROOT/dist/json/hydra-kernel" ]; then
    echo "ERROR: dist/json/hydra-kernel not found. Run bin/sync.sh (or bin/sync-java.sh)" >&2
    echo "       first to populate dist/json." >&2
    exit 1
fi

# All pages this generator can produce (primitives/ + types/), independent of
# PAGE_MODULES' internal mapping -- just every *.md under the two dirs except
# index.md (conventions doc, never generated) and the two structurally-excluded
# type-class catalogs (equality.md/ordering.md -- see
# .claude/commands/regenerate-spec.md step 4).
ALL_PAGES=()
while IFS= read -r -d '' f; do
    rel="${f#"$SPEC_DIR"/}"
    case "$rel" in
        primitives/index.md|primitives/equality.md|primitives/ordering.md) continue ;;
    esac
    ALL_PAGES+=("$rel")
done < <(find "$SPEC_DIR/primitives" "$SPEC_DIR/types" -maxdepth 1 -name '*.md' -print0 | sort -z)

CONVERGED_PAGES=()
if [ -f "$CONVERGED_LIST" ]; then
    while IFS= read -r line; do
        [ -z "$line" ] && continue
        case "$line" in \#*) continue ;; esac
        CONVERGED_PAGES+=("$line")
    done < "$CONVERGED_LIST"
fi
is_converged() {
    local page="$1"
    local p
    for p in "${CONVERGED_PAGES[@]+"${CONVERGED_PAGES[@]}"}"; do
        [ "$p" = "$page" ] && return 0
    done
    return 1
}

# Back up every in-scope page so we can restore after the check -- this script
# inspects freshness, it does not commit a regen (that is bin/regenerate-spec.sh's
# job, run deliberately and reviewed per .claude/commands/regenerate-spec.md).
BACKUP_DIR="$(mktemp -d)"
trap 'rm -rf "$BACKUP_DIR"' EXIT
for rel in "${ALL_PAGES[@]}"; do
    mkdir -p "$BACKUP_DIR/$(dirname "$rel")"
    cp "$SPEC_DIR/$rel" "$BACKUP_DIR/$rel"
done

echo "Regenerating docs/specification/{primitives,types}/ pages for freshness check..."
if ! "$HYDRA_ROOT/bin/regenerate-spec.sh" > /tmp/check-spec-freshness.regen.log 2>&1; then
    echo "ERROR: bin/regenerate-spec.sh exited non-zero. Last 30 lines:" >&2
    tail -30 /tmp/check-spec-freshness.regen.log >&2
    # Restore before exiting -- a failed regen must not leave partial output committed.
    for rel in "${ALL_PAGES[@]}"; do
        cp "$BACKUP_DIR/$rel" "$SPEC_DIR/$rel"
    done
    exit 1
fi

CONVERGED_NOW=()
NOT_YET=()
REGRESSED=()
for rel in "${ALL_PAGES[@]}"; do
    if diff -q "$BACKUP_DIR/$rel" "$SPEC_DIR/$rel" > /dev/null 2>&1; then
        CONVERGED_NOW+=("$rel")
    else
        if is_converged "$rel"; then
            REGRESSED+=("$rel")
        else
            NOT_YET+=("$rel")
        fi
    fi
done

# Restore the committed pages -- this script only checks, never writes.
for rel in "${ALL_PAGES[@]}"; do
    cp "$BACKUP_DIR/$rel" "$SPEC_DIR/$rel"
done

echo ""
echo "=== Spec page freshness ==="
echo "Converged (${#CONVERGED_NOW[@]}):"
for rel in "${CONVERGED_NOW[@]+"${CONVERGED_NOW[@]}"}"; do echo "  OK   $rel"; done
echo "Not yet converged (${#NOT_YET[@]}):"
for rel in "${NOT_YET[@]+"${NOT_YET[@]}"}"; do echo "  ...  $rel"; done
if [ "${#REGRESSED[@]}" -gt 0 ]; then
    echo "REGRESSED (${#REGRESSED[@]}) -- listed in CONVERGED-PAGES.txt but no longer matches:"
    for rel in "${REGRESSED[@]}"; do echo "  FAIL $rel"; done
fi
echo ""

if [ "${#REGRESSED[@]}" -gt 0 ]; then
    echo "FAIL: ${#REGRESSED[@]} page(s) regressed. Run bin/regenerate-spec.sh, review the diff," >&2
    echo "      and either fix the drift or (if the hand-authored page was deliberately updated" >&2
    echo "      and the generator needs to catch up) update the kernel source accordingly." >&2
    exit 1
fi

echo "OK: no regressions. ${#CONVERGED_NOW[@]}/${#ALL_PAGES[@]} pages converged."
