#!/usr/bin/env bash
# Layer 2 batch assembler: produce python distributions for every
# package in a single bootstrap-from-json invocation. Much faster than
# calling assemble-distribution.sh once per package because the JSON
# universe is loaded only once.
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
source "$HYDRA_ROOT_DIR/bin/lib/batch-cache.sh"
if batch_cache_fresh "$DIST_ROOT" "$HYDRA_ROOT_DIR/dist/json"; then
    echo "  Cache hit: every per-package digest fresh; skipping batch."
    echo "=== Done (cache hit). ==="
    exit 0
fi

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

# Per-package post-processing for hydra-kernel: copy test_env.py.
# (test_graph.py post-generation patch eliminated: the DSL now emits
# test_graph = lambda: test_env.test_graph(test_types()) directly, via
# the FQN stubs in Hydra.Sources.Test.TestEnv. See task #25 in the
# feature_290_packaging plan.)
TEST_ENV_SRC="$HEAD_DIR/src/test/python/hydra/test/test_env.py"
TEST_ENV_DST="$DIST_ROOT/hydra-kernel/src/test/python/hydra/test/test_env.py"
if [ -f "$TEST_ENV_SRC" ]; then
    echo ""
    echo "Step 3a: Copying test_env.py from heads/python/..."
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

# Refresh per-source-set digests for fresh-check cache.
for pkg_dir in "$DIST_ROOT"/*/; do
    pkg=$(basename "$pkg_dir")
    pkg_dir_trim="${pkg_dir%/}"
    for set_name in main test; do
        input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/src/$set_name/digest.json"
        out_set_dir="$pkg_dir_trim/src/$set_name/python"
        out_digest="$pkg_dir_trim/src/$set_name/digest.json"
        if [ -f "$input_digest" ] && [ -d "$out_set_dir" ]; then
            (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
             stack exec digest-check -- refresh \
                --inputs "$input_digest" \
                --output-dir "$out_set_dir" \
                --output-digest "$out_digest")
        fi
    done
done

echo ""
echo "=== Done. Batch python assembly complete under $DIST_ROOT ==="
