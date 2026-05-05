#!/usr/bin/env bash
# Layer 2 batch assembler: produce Java distributions for every package
# in a single bootstrap-from-json invocation. Much faster than calling
# assemble-distribution.sh once per package because the JSON universe is
# loaded only once.
#
# Usage:
#   assemble-all.sh [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ for every package in one shot. Per-package
# post-processing matches assemble-distribution.sh.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_JAVA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_JAVA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/java"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

echo "=== Assembling Java distributions (batch mode, all packages) ==="
echo "  Output root: $DIST_ROOT"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Warm-cache short-circuit: skip BEFORE any stack invocation or digest wipe.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

BATCH_PACKAGES=$(batch_emit_packages)

# Wipe per-package generated source dirs before regenerating, so that
# stale modules left behind by deleted/renamed sources don't survive.
# bootstrap-from-json writes only the modules currently in the
# dist/json universe; without this wipe, an older generated file with
# no upstream JSON source would persist, get hashed into the digest,
# and silently become part of the build. See follow-up #N for the
# generator-side fix that would make this redundant.
#
# This wipes only src/{main,test}/java/. Per-package build.gradle and
# settings.gradle (in $DIST_ROOT/<pkg>/) are preserved; the kernel's
# hand-written runtime files copied in by copy-kernel-runtime.sh are
# under src/main/java/ and so DO get wiped here, but Step 3 re-copies
# them after generation.
echo "Wiping per-package generated source dirs..."
for pkg in $BATCH_PACKAGES; do
    rm -rf "$DIST_ROOT/$pkg/src/main/java" "$DIST_ROOT/$pkg/src/test/java"
done

# Invalidate per-target digests so Stage 7 per-module freshness filter
# cannot trust records against potentially-missing output files.
rm -f "$DIST_ROOT"/*/src/main/digest.json "$DIST_ROOT"/*/src/test/digest.json

echo "Step 1: Generating main Java modules for every package..."
cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1
stack exec bootstrap-from-json -- \
    --target java \
    --all-packages \
    --include-coders --include-dsls \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test Java modules..."
stack exec bootstrap-from-json -- \
    --target java \
    --all-packages \
    --include-coders --include-dsls --include-tests \
    --output "$DIST_ROOT"

cd "$HYDRA_ROOT_DIR"

# Step 3: Per-package post-processing. Mirrors assemble-distribution.sh
# Step 3 — hydra-kernel only today. Must run BEFORE the digest refresh
# below: copy-kernel-runtime.sh writes into dist/java/hydra-kernel/src/main/java/,
# which the digest tracks; running it after refresh would leave those
# hand-written files unrecorded and trigger a needless re-run on the
# next 'fresh' check.
echo ""
echo "Step 3: Copying hand-written Java runtime into hydra-kernel dist..."
"$SCRIPT_DIR/copy-kernel-runtime.sh" --dist-root "$DIST_ROOT"

# Step 4: Generate per-package build.gradle + settings.gradle. Mirrors
# assemble-distribution.sh Step 4. The build files live at
# dist/java/<pkg>/{build.gradle,settings.gradle} — outside the
# src/<set>/java tree the digest tracks — so ordering with digest
# refresh is moot.
#
# Package list ($BATCH_PACKAGES) is the batch emit set (baseline +
# coders): the same packages bootstrap-from-json wrote source for
# above. The ext and ext-demo packages (hydra-pg, hydra-rdf,
# hydra-ext, ...) get their build files from the per-package
# assemble-distribution.sh path, which CI runs separately after this
# batch.
echo ""
echo "Step 4: Generating per-package build.gradle for every batch-emitted package..."
for pkg in $BATCH_PACKAGES; do
    HYDRA_ROOT_DIR="$HYDRA_ROOT_DIR" "$HYDRA_ROOT_DIR/bin/lib/generate-java-package-build.py" \
        "$pkg" --out-dir "$DIST_ROOT/$pkg"
done

# Refresh per-source-set digests for fresh-check cache. Driven by the
# batch emit set above, not by walking dist: every package the batch
# generator emitted must have a main source set on disk, so a missing
# dist/java/<pkg>/src/main/java/ is an error (signal of a broken
# bootstrap-from-json pass) rather than something to silently skip.
# The test source set is optional — gated on whether the input test
# digest at dist/json/<pkg>/src/test/digest.json exists.
for pkg in $BATCH_PACKAGES; do
    pkg_dir="$DIST_ROOT/$pkg"
    # Main set: required.
    input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/main/digest.json"
    out_set_dir="$pkg_dir/src/main/java"
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
        test_out_set_dir="$pkg_dir/src/test/java"
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
echo "=== Done. Batch Java assembly complete under $DIST_ROOT ==="
