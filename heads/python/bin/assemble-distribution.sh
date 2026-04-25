#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Python distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/python/<pkg>/) by:
#   1. Calling Layer 1 transform-json-to-python.sh for main modules
#   2. Calling Layer 1 transform-json-to-python.sh for test modules
#   3. Applying package-specific post-processing:
#      - hydra-kernel: copy test_env.py, patch test_graph.py
#
# Assemblers do NOT run tests. Test invocation is Layer 2.5's
# test-distribution.sh. See feature_290_packaging-plan.md.
#
# Note: pyproject.toml currently lives in heads/python/, not per-dist, so no
# build-file generation happens per package. Future work: per-package
# pyproject.toml templates.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_PYTHON_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/python"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/python"
OUT_TEST="$OUT_DIR/src/test/python"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Python distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Python modules..."
    "$HASKELL_BIN/transform-json-to-python.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls
    assemble_refresh_digest "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"
fi

# Step 2: Test modules. Any package can have a test source set (just
# `dist/json/<pkg>/src/test/json/`); the per-source-set digest mechanism
# is uniform — adding a test dir for any package automatically wires it
# into the build.
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    if assemble_check_fresh "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        rm -f "$OUTPUT_DIGEST_TEST"
        echo "Step 2: Generating test Python modules..."
        "$HASKELL_BIN/transform-json-to-python.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Copy hand-written hydra/lib/ and hydra/dsl/ from heads/python into dist/.
        # heads/python is the source of truth for these helpers; dist mirrors it
        # so that downstream consumers importing from dist/python/hydra-kernel
        # see the same surface as the bootstrap demo.
        for d in lib dsl; do
            LIB_SRC="$HYDRA_PYTHON_HEAD/src/main/python/hydra/$d"
            LIB_DST="$OUT_MAIN/hydra/$d"
            if [ -d "$LIB_SRC" ]; then
                echo "Step 3a: Copying hand-written hydra/$d/ from heads/python/..."
                mkdir -p "$LIB_DST"
                cp -R "$LIB_SRC/." "$LIB_DST/"
                find "$LIB_DST" -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
            fi
        done

        # Copy test_env.py from heads/python into dist/.
        # The patched test_graph.py below imports hydra.test.test_env, which
        # must resolve under the dist tree at test time.
        TEST_ENV_SRC="$HYDRA_PYTHON_HEAD/src/test/python/hydra/test/test_env.py"
        TEST_ENV_DST="$OUT_TEST/hydra/test/test_env.py"
        if [ -f "$TEST_ENV_SRC" ]; then
            echo ""
            echo "Step 3b: Copying test_env.py from heads/python/..."
            mkdir -p "$(dirname "$TEST_ENV_DST")"
            cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
        fi

        # Patch test_graph.py: replace module-level test_graph/test_context
        # with a lazy __getattr__ shim that defers construction. Without the
        # shim, test_env imports fail at module load time.
        TESTGRAPH="$OUT_TEST/hydra/test/test_graph.py"
        if [ -f "$TESTGRAPH" ]; then
            echo "Step 3c: Patching test_graph.py..."
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
        ;;
    *)
        # No per-package post-processing for other packages today.
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
