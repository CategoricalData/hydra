#!/usr/bin/env bash
# Layer 2 batch assembler: produce haskell distributions for every
# package in a single bootstrap-from-json invocation. Much faster than
# calling assemble-distribution.sh once per package because the JSON
# universe is loaded only once.
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HEAD_DIR/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/haskell"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

echo "=== Assembling haskell distributions (batch mode, all packages) ==="
echo "  Output root: $DIST_ROOT"
echo ""

# Warm-cache short-circuit: skip BEFORE any stack invocation.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
export HYDRA_GENERATOR_STAMP=$(compute_generator_stamp haskell)
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

BATCH_PACKAGES=$(batch_emit_packages)

# Stale-file pruning is handled by bootstrap-from-json --prune-stale (#357).
cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json hydra:exe:digest-check >/dev/null 2>&1

# Invalidate per-target digests so Stage 7 can't trust stale records.
# Scoped to $BATCH_PACKAGES — packages outside the batch emit set
# (e.g. hydra-pg, hydra-ext) keep their digests untouched; those are
# managed by per-package assemble-distribution.sh runs.
for pkg in $BATCH_PACKAGES; do
    rm -f "$DIST_ROOT/$pkg/src/main/digest.json" "$DIST_ROOT/$pkg/src/test/digest.json"
done

echo "Step 1: Generating main haskell modules for every package..."
# --include-ext is omitted: hydra-ext's targetLanguages does not include
# haskell, so it's outside the batch matrix. Per-package regen via
# heads/haskell/bin/assemble-distribution.sh hydra-ext is supported.
stack exec bootstrap-from-json -- \
    --target haskell \
    --all-packages \
    --include-coders --include-dsls --synthesize-sources \
    --prune-stale \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test haskell modules..."
stack exec bootstrap-from-json -- \
    --target haskell \
    --all-packages \
    --include-coders --include-dsls --include-tests \
    --prune-stale \
    --output "$DIST_ROOT"

# No per-package post-processing today — the generator emits
# Hydra.Test.TestEnv references directly. See docs/recipes/maintenance.md
# "Known accepted patches" for the history.

cd "$HYDRA_ROOT_DIR"

# Refresh per-source-set digests for fresh-check cache. Driven by
# $BATCH_PACKAGES, not by walking dist: every package the batch
# generator emitted must have a main source set on disk, so a missing
# dist/haskell/<pkg>/src/main/haskell/ is an error (signal of a broken
# bootstrap-from-json pass) rather than something to silently skip.
# The test source set is optional — gated on whether the input test
# digest at dist/json/<pkg>/src/test/digest.json exists.
for pkg in $BATCH_PACKAGES; do
    pkg_dir="$DIST_ROOT/$pkg"
    # Main set: required.
    input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/main/digest.json"
    out_set_dir="$pkg_dir/src/main/haskell"
    out_digest="$pkg_dir/src/main/digest.json"
    if [ ! -f "$input_digest" ]; then
        echo "ERROR: missing input digest for $pkg main: $input_digest" >&2
        exit 1
    fi
    if [ ! -d "$out_set_dir" ]; then
        echo "ERROR: missing generated output for $pkg main: $out_set_dir" >&2
        exit 1
    fi
    (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
     stack exec digest-check -- refresh \
        --inputs "$input_digest" \
        --output-dir "$out_set_dir" \
        --output-digest "$out_digest")
    # Test set: optional, gated on input test digest presence.
    test_input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/test/digest.json"
    if [ -f "$test_input_digest" ]; then
        test_out_set_dir="$pkg_dir/src/test/haskell"
        test_out_digest="$pkg_dir/src/test/digest.json"
        if [ ! -d "$test_out_set_dir" ]; then
            echo "ERROR: missing generated test output for $pkg: $test_out_set_dir" >&2
            exit 1
        fi
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- refresh \
            --inputs "$test_input_digest" \
            --output-dir "$test_out_set_dir" \
            --output-digest "$test_out_digest")
    fi
done

echo ""
echo "=== Done. Batch haskell assembly complete under $DIST_ROOT ==="
