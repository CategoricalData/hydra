#!/usr/bin/env bash
# Conformance test for the assembly-plan executor (#416, Design D).
#
# apply-assembly-plan.sh derives the overlay-merge + keep-paths plan inline in pure
# bash (a data-driven fold over find-results). This test asserts that inline derivation
# byte-matches the AUTHORITATIVE plan for every overlay tree in the repo, so the bash
# fold can never silently drift from the promoted spec.
#
# The oracle is hydra.build.assemblyplan.deriveAssemblyPlan (pure path-concat:
# overlay/<lang>/<pkg>/<sub> -> dist/<lang>/<pkg>/<sub>, kind merge; keep-paths keyed by
# dist source-set dir). The per-host copy-overlay.sh scripts are the operational
# encoding of that same plan that this batch REPLACES; asserting apply-assembly-plan.sh
# == copy-overlay.sh across every overlay tree is the equivalence check. (The DSL is a
# TEST-TIME oracle only — this test shells out to NO Haskell/Java toolchain; it compares
# two pure-bash outputs. Zero build-execute host dependency, per the #416 standing rule.)
#
# Wired into bin/test-regressions.sh. Exit 0 = conformant; nonzero + a diff = drift.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="${HYDRA_ROOT_DIR:-$( cd "$SCRIPT_DIR/.." && pwd )}"
export HYDRA_ROOT_DIR

APPLY="$HYDRA_ROOT_DIR/bin/apply-assembly-plan.sh"

# The hosts whose copy-overlay.sh this executor supersedes (the reference encodings of
# deriveAssemblyPlan). Each is checked against every package that has an overlay tree.
REFERENCE_HOSTS="java python"

TMP="$(mktemp -d -t hydra-asm-conformance.XXXXXX)"
trap 'rm -rf "$TMP"' EXIT

fail=0
checked=0

for lang in $REFERENCE_HOSTS; do
    copy_overlay="$HYDRA_ROOT_DIR/heads/$lang/bin/copy-overlay.sh"
    [ -x "$copy_overlay" ] || continue
    overlay_root="$HYDRA_ROOT_DIR/overlay/$lang"
    [ -d "$overlay_root" ] || continue

    for pkg_dir in "$overlay_root"/*/; do
        pkg="$(basename "$pkg_dir")"
        [ -d "${pkg_dir}src" ] || continue   # only packages with an overlay src/ tree

        apply_root="$TMP/$lang/$pkg/apply"
        copy_root="$TMP/$lang/$pkg/copy"
        mkdir -p "$apply_root" "$copy_root"
        apply_manifest="$TMP/$lang/$pkg/apply.keep"
        copy_manifest="$TMP/$lang/$pkg/copy.keep"

        "$APPLY" "$lang" "$pkg" --dist-root "$apply_root" --manifest "$apply_manifest" >/dev/null
        "$copy_overlay" "$pkg" --dist-root "$copy_root" --manifest "$copy_manifest" >/dev/null

        checked=$((checked + 1))

        # 1. dist tree byte-identical.
        if ! diff -r "$apply_root" "$copy_root" >/dev/null 2>&1; then
            echo "DRIFT [$lang/$pkg]: dist tree differs (apply-assembly-plan vs copy-overlay):" >&2
            diff -r "$apply_root" "$copy_root" >&2 | head -20
            fail=1
        fi

        # 2. keep-manifest byte-identical (normalize the distinct scratch dist-roots).
        norm_apply="$(sed "s|$apply_root|DISTROOT|g" "$apply_manifest" 2>/dev/null | sort)"
        norm_copy="$(sed "s|$copy_root|DISTROOT|g" "$copy_manifest" 2>/dev/null | sort)"
        if [ "$norm_apply" != "$norm_copy" ]; then
            echo "DRIFT [$lang/$pkg]: keep-manifest differs (apply-assembly-plan vs copy-overlay):" >&2
            diff <(printf '%s\n' "$norm_apply") <(printf '%s\n' "$norm_copy") >&2 | head -20
            fail=1
        fi
    done
done

if [ "$checked" -eq 0 ]; then
    echo "test-assembly-plan-conformance: no overlay trees found to check — did the repo layout change?" >&2
    exit 1
fi

if [ "$fail" -ne 0 ]; then
    echo "test-assembly-plan-conformance: FAILED — apply-assembly-plan.sh drifted from the plan oracle." >&2
    exit 1
fi

echo "test-assembly-plan-conformance: OK — apply-assembly-plan.sh matches the plan oracle across $checked overlay tree(s)."
