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

cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1

# Invalidate per-target digests so Stage 7 can't trust stale records.
rm -f "$DIST_ROOT"/*/digest.json

echo "Step 1: Generating main python modules for every package..."
stack exec bootstrap-from-json -- \
    --target python \
    --all-packages \
    --include-coders --include-ext --include-dsls \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test python modules..."
stack exec bootstrap-from-json -- \
    --target python \
    --all-packages \
    --include-coders --include-ext --include-dsls --include-tests \
    --output "$DIST_ROOT"

cd "$HYDRA_ROOT_DIR"

# Per-package post-processing for hydra-kernel: copy test_env.py and patch
# test_graph.py with a lazy __getattr__ shim.
TEST_ENV_SRC="$HEAD_DIR/src/test/python/hydra/test/test_env.py"
TEST_ENV_DST="$DIST_ROOT/hydra-kernel/src/test/python/hydra/test/test_env.py"
if [ -f "$TEST_ENV_SRC" ]; then
    echo ""
    echo "Step 3a: Copying test_env.py from heads/python/..."
    mkdir -p "$(dirname "$TEST_ENV_DST")"
    cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
fi

TESTGRAPH="$DIST_ROOT/hydra-kernel/src/test/python/hydra/test/test_graph.py"
if [ -f "$TESTGRAPH" ]; then
    echo "Step 3b: Patching test_graph.py..."
    sed -i.bak '/^test_context = /d' "$TESTGRAPH"
    sed -i.bak '/^test_graph = /d' "$TESTGRAPH"
    rm -f "$TESTGRAPH.bak"
    cat >> "$TESTGRAPH" << 'PYEOF'

_test_graph_cache = None
_test_context_cache = None

def __getattr__(name):
    global _test_graph_cache, _test_context_cache
    if name == "test_graph":
        if _test_graph_cache is None:
            import hydra.test.test_env as _test_env
            _test_graph_cache = _test_env.test_graph()
        return _test_graph_cache
    elif name == "test_context":
        if _test_context_cache is None:
            import hydra.test.test_env as _test_env
            _test_context_cache = _test_env.test_context()
        return _test_context_cache
    raise AttributeError(f"module 'hydra.test.test_graph' has no attribute {name!r}")
PYEOF
fi

# Refresh per-target digests for fresh-check cache.
for pkg_dir in "$DIST_ROOT"/*/; do
    pkg=$(basename "$pkg_dir")
    input_digest="$HYDRA_ROOT_DIR/dist/json/$pkg/digest.json"
    output_digest="$pkg_dir/digest.json"
    if [ -f "$input_digest" ]; then
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- refresh \
            --inputs "$input_digest" \
            --output-dir "$pkg_dir" \
            --output-digest "$output_digest")
    fi
done

echo ""
echo "=== Done. Batch python assembly complete under $DIST_ROOT ==="
