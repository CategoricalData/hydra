#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Haskell distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>] [--json-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/haskell/<pkg>/) by:
#   1. Calling Layer 1 transform-json-to-haskell.sh for main modules
#   2. Calling Layer 1 transform-json-to-haskell.sh for test modules (if the
#      package has any)
#   3. Applying package-specific post-processing (kernel: TestGraph patch)
#
# Assemblers do NOT run tests. Testing is Layer 2.5's job
# (test-distribution.sh), invoked separately. See
# feature_290_packaging-plan.md, "Sync system redesign / Layer 2".
#
# Note: the Haskell head has no per-package build file to generate — the
# monolithic packages/hydra-haskell/hydra.cabal references every dist subdir
# via its source-dirs list. Future work: per-package .cabal files.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>] [--json-root <dir>]" >&2
    echo "" >&2
    echo "Packages: hydra-kernel, hydra-haskell, hydra-java, hydra-python," >&2
    echo "          hydra-scala, hydra-lisp, hydra-pg, hydra-rdf," >&2
    echo "          hydra-coq, hydra-javascript, hydra-ext" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/haskell"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        --json-root) shift 2 ;;  # forwarded to Layer 1 via --dist-json-root
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
INPUT_DIGEST="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/digest.json"
OUTPUT_DIGEST="$OUT_DIR/digest.json"

echo "=== Assembling Haskell distribution: $PACKAGE ==="
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
    if (cd "$HYDRA_HASKELL_DIR" && \
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

# Step 1: Main modules via Layer 1 transform. Routing is unconditional:
# every module lands under <DIST_ROOT>/<owning-pkg>/ based on its namespace,
# including transitive dependencies for this package. The output dir argument
# is the parent of the per-package dirs.
#
# --synthesize-sources generates hand-equivalent Hydra.Sources.Decode.* and
# Hydra.Sources.Encode.* modules from type definitions. The synthesis filter
# picks up kernel type modules and the two hydra-pg type modules
# (hydra.pg.model, hydra.pg.mapping). hydra-pg's assembler passes the flag so
# its own source wrappers land in dist/haskell/hydra-pg/.
SYNTH_FLAG=""
case "$PACKAGE" in
    hydra-kernel|hydra-pg)
        SYNTH_FLAG="--synthesize-sources"
        ;;
esac

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
MAIN_HASH=$(hash_dir "$MAIN_JSON_DIR")
RECORDED_MAIN=""
[ -f "$MAIN_INPUT_HASH_FILE" ] && RECORDED_MAIN=$(cat "$MAIN_INPUT_HASH_FILE")
if [ -n "$MAIN_HASH" ] && [ "$MAIN_HASH" = "$RECORDED_MAIN" ]; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    echo "Step 1: Generating main Haskell modules..."
    "$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls $SYNTH_FLAG
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
        echo "Step 2: Generating test Haskell modules..."
        "$SCRIPT_DIR/transform-json-to-haskell.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        mkdir -p "$OUT_DIR" && echo "$TEST_HASH" > "$TEST_INPUT_HASH_FILE"
    fi
fi

# Step 3: Package-specific post-processing. None today — the generator emits
# Hydra.Test.TestEnv references directly; see sync-haskell.sh step 5 and
# docs/recipes/maintenance.md "Known accepted patches".

# Refresh the per-target digest so future fresh-checks short-circuit.
if [ -f "$INPUT_DIGEST" ]; then
    (cd "$HYDRA_HASKELL_DIR" && \
     stack exec digest-check -- refresh \
        --inputs "$INPUT_DIGEST" \
        --output-dir "$OUT_DIR" \
        --output-digest "$OUTPUT_DIGEST")
fi

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
