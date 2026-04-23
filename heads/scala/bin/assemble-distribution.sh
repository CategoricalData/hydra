#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Scala distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/scala/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_SCALA_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_SCALA_HEAD/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/scala"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
INPUT_DIGEST="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/digest.json"
OUTPUT_DIGEST="$OUT_DIR/digest.json"

echo "=== Assembling Scala distribution: $PACKAGE ==="
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

# Per-source-set caches: skip regeneration of main and test source sets
# independently. See heads/java/bin/assemble-distribution.sh for the
# pattern; same shape across every target language.
MAIN_INPUT_HASH_FILE="$OUT_DIR/.main-input-hash.txt"
TEST_INPUT_HASH_FILE="$OUT_DIR/.test-input-hash.txt"
MAIN_JSON_DIR="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/src/main/json"
TEST_JSON_DIR="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/src/test/json"
hash_dir() {
    local d="$1"
    if [ -d "$d" ]; then
        find "$d" -type f -name '*.json' 2>/dev/null \
            | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null \
            | shasum -a 256 | awk '{print $1}'
    else
        echo ""
    fi
}

# Step 1: Main modules.
# bootstrap-from-json appends <pkg>/src/main/<target> to --output, so we
# pass the dist-root directory (parent of per-package dirs).
MAIN_HASH=$(hash_dir "$MAIN_JSON_DIR")
RECORDED_MAIN=""
[ -f "$MAIN_INPUT_HASH_FILE" ] && RECORDED_MAIN=$(cat "$MAIN_INPUT_HASH_FILE")
if [ -n "$MAIN_HASH" ] && [ "$MAIN_HASH" = "$RECORDED_MAIN" ]; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    echo "Step 1: Generating main Scala modules..."
    "$HASKELL_BIN/transform-json-to-scala.sh" "$PACKAGE" main \
        --output "$DIST_ROOT"
    [ -n "$MAIN_HASH" ] && mkdir -p "$OUT_DIR" && echo "$MAIN_HASH" > "$MAIN_INPUT_HASH_FILE"
fi

# Step 2: Test modules. Any package can have a test source set; today
# only hydra-kernel does, but the mechanism is uniform.
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    TEST_HASH=$(hash_dir "$TEST_JSON_DIR")
    RECORDED_TEST=""
    [ -f "$TEST_INPUT_HASH_FILE" ] && RECORDED_TEST=$(cat "$TEST_INPUT_HASH_FILE")
    if [ "$TEST_HASH" = "$RECORDED_TEST" ]; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        echo "Step 2: Generating test Scala modules..."
        "$HASKELL_BIN/transform-json-to-scala.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        mkdir -p "$OUT_DIR" && echo "$TEST_HASH" > "$TEST_INPUT_HASH_FILE"
    fi
fi

# Step 3: Package-specific post-processing.
# (testGraph.scala emptyGraph-to-buildTestGraph patch eliminated: the DSL now
# emits `hydra.test.testEnv.testGraph(testTypes)` directly, and the hand-written
# heads/scala testEnv.scala resolves the call to TestSuiteRunner.buildTestGraph.)
case "$PACKAGE" in
    *)
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
