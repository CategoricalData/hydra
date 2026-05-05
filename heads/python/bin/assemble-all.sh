#!/usr/bin/env bash
# Layer 2 batch assembler: produce python distributions for every
# package in a single bootstrap-from-json invocation. Much faster than
# calling assemble-distribution.sh once per package because the JSON
# universe is loaded only once.
#
# The per-package post-processing applied below MUST stay in sync with
# heads/python/bin/assemble-distribution.sh Step 3, since both scripts
# are entry points for sync.sh (per-package) and sync-packages.sh (batch).
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HEAD_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HEAD_DIR/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/python"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

echo "=== Assembling python distributions (batch mode, all packages) ==="
echo "  Output root: $DIST_ROOT"
echo ""

# Warm-cache short-circuit: skip BEFORE any stack invocation.
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
# and silently become part of the build.
#
# This wipes only src/{main,test}/python/. Per-package pyproject.toml
# (in $DIST_ROOT/<pkg>/) is preserved; the kernel's hand-written
# runtime files copied in by copy-kernel-runtime.sh are under
# src/main/python/ and so DO get wiped here, but Step 3b re-copies
# them after generation.
echo "Wiping per-package generated source dirs..."
for pkg in $BATCH_PACKAGES; do
    rm -rf "$DIST_ROOT/$pkg/src/main/python" "$DIST_ROOT/$pkg/src/test/python"
done

cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1

# Invalidate per-target digests so Stage 7 can't trust stale records.
rm -f "$DIST_ROOT"/*/src/main/digest.json "$DIST_ROOT"/*/src/test/digest.json

echo "Step 1: Generating main python modules for every package..."
stack exec bootstrap-from-json -- \
    --target python \
    --all-packages \
    --include-coders --include-dsls \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test python modules..."
stack exec bootstrap-from-json -- \
    --target python \
    --all-packages \
    --include-coders --include-dsls --include-tests \
    --output "$DIST_ROOT"

cd "$HYDRA_ROOT_DIR"

# Per-package post-processing for hydra-kernel: copy test_env.py and
# the hand-written Python runtime. Must mirror
# heads/python/bin/assemble-distribution.sh Step 3 — both entrypoints
# (sync.sh per-package and sync-packages.sh batch) must produce the
# same dist tree.
TEST_ENV_SRC="$HEAD_DIR/src/test/python/hydra/test/test_env.py"
TEST_ENV_DST="$DIST_ROOT/hydra-kernel/src/test/python/hydra/test/test_env.py"
if [ -f "$TEST_ENV_SRC" ]; then
    echo ""
    echo "Step 3a: Copying test_env.py from heads/python/..."
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

# Copy hand-written Python runtime so the published kernel wheel is
# self-contained.
echo ""
echo "Step 3b: Copying hand-written Python runtime into hydra-kernel dist..."
"$SCRIPT_DIR/copy-kernel-runtime.sh" --dist-root "$DIST_ROOT"

# Step 4: Generate per-package pyproject.toml so each dist/python/<pkg>/
# is a standalone publishable wheel build. Mirrors
# assemble-distribution.sh Step 4. The pyproject.toml lives at
# dist/python/<pkg>/pyproject.toml — outside the src/<set>/python tree
# the digest tracks — so ordering with digest refresh is moot.
#
# Package list ($BATCH_PACKAGES) is the batch emit set (baseline +
# coders). Ext / ext-demo packages get their build files from the
# per-package assemble-distribution.sh path.
echo ""
echo "Step 4: Generating per-package pyproject.toml for every batch-emitted package..."
for pkg in $BATCH_PACKAGES; do
    HYDRA_ROOT_DIR="$HYDRA_ROOT_DIR" "$HYDRA_ROOT_DIR/bin/lib/generate-python-package-build.py" \
        "$pkg" --out-dir "$DIST_ROOT/$pkg"
done

# Refresh per-source-set digests for fresh-check cache. Driven by the
# batch emit set above, not by walking dist: every package the batch
# generator emitted must have a main source set on disk, so a missing
# dist/python/<pkg>/src/main/python/ is an error (signal of a broken
# bootstrap-from-json pass) rather than something to silently skip.
# The test source set is optional — gated on whether the input test
# digest at dist/json/<pkg>/src/test/digest.json exists.
for pkg in $BATCH_PACKAGES; do
    pkg_dir="$DIST_ROOT/$pkg"
    # Main set: required.
    input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/main/digest.json"
    out_set_dir="$pkg_dir/src/main/python"
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
        test_out_set_dir="$pkg_dir/src/test/python"
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
echo "=== Done. Batch python assembly complete under $DIST_ROOT ==="
