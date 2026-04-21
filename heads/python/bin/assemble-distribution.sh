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
INPUT_DIGEST="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/digest.json"
OUTPUT_DIGEST="$OUT_DIR/digest.json"

echo "=== Assembling Python distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

# Cheap Python pre-check: compare input digest hashes to recorded
# output digest inputs. Avoids stack-exec startup for warm runs.
if [ -f "$INPUT_DIGEST" ] && [ -f "$OUTPUT_DIGEST" ]; then
    if python3 -c "
import json, sys
try:
    out = json.load(open('$OUTPUT_DIGEST'))
    inp = json.load(open('$INPUT_DIGEST'))
    recorded = {k: (v.get('hash') if isinstance(v, dict) else v)
                for k, v in out.get('inputs', {}).items()}
    current = inp.get('hashes', inp)
    sys.exit(0 if recorded == current else 1)
except Exception:
    sys.exit(1)
" 2>/dev/null; then
        echo "  Cache hit; skipping work."
        echo "=== Done. $PACKAGE (cache hit) ==="
        exit 0
    fi
fi

# Freshness check: skip the slow path when nothing has changed.
if [ -f "$INPUT_DIGEST" ] && [ -f "$OUTPUT_DIGEST" ]; then
    if (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
        stack exec digest-check -- fresh \
            --inputs "$INPUT_DIGEST" \
            --output-dir "$OUT_DIR" \
            --output-digest "$OUTPUT_DIGEST" 2>/dev/null); then
        echo "  Cache hit; skipping work."
        echo "=== Done. $PACKAGE assembled under $OUT_DIR (cache hit) ==="
        exit 0
    fi
fi

# Cache miss: invalidate the per-target digest so Stage 7's per-module
# freshness filter inside bootstrap-from-json can't trust stale records
# (output files may be missing/modified, so DSL-hash-only freshness is
# unreliable).
rm -f "$OUTPUT_DIGEST"

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Step 1: Main modules via Layer 1 transform.
# bootstrap-from-json appends <pkg>/src/main/<target> to --output, so we
# pass the dist-root directory (parent of per-package dirs).
echo "Step 1: Generating main Python modules..."
"$HASKELL_BIN/transform-json-to-python.sh" "$PACKAGE" main \
    --output "$DIST_ROOT" --include-dsls

# Step 2: Test modules.
echo ""
echo "Step 2: Generating test Python modules..."
"$HASKELL_BIN/transform-json-to-python.sh" "$PACKAGE" test \
    --output "$DIST_ROOT"

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Copy test_env.py from heads/python into dist/.
        # The patched test_graph.py below imports hydra.test.test_env, which
        # must resolve under the dist tree at test time.
        TEST_ENV_SRC="$HYDRA_PYTHON_HEAD/src/test/python/hydra/test/test_env.py"
        TEST_ENV_DST="$OUT_TEST/hydra/test/test_env.py"
        if [ -f "$TEST_ENV_SRC" ]; then
            echo ""
            echo "Step 3a: Copying test_env.py from heads/python/..."
            mkdir -p "$(dirname "$TEST_ENV_DST")"
            cp "$TEST_ENV_SRC" "$TEST_ENV_DST"
        fi

        # Patch test_graph.py: replace module-level test_graph/test_context
        # with a lazy __getattr__ shim that defers construction. Without the
        # shim, test_env imports fail at module load time.
        TESTGRAPH="$OUT_TEST/hydra/test/test_graph.py"
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
        ;;
    *)
        # No per-package post-processing for other packages today.
        ;;
esac

echo ""

# Refresh the per-target digest so future fresh-checks short-circuit.
if [ -f "$INPUT_DIGEST" ]; then
    (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
     stack exec digest-check -- refresh \
        --inputs "$INPUT_DIGEST" \
        --output-dir "$OUT_DIR" \
        --output-digest "$OUTPUT_DIGEST")
fi

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
