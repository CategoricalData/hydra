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
OUT_MAIN="$OUT_DIR/src/main/scala"
OUT_TEST="$OUT_DIR/src/test/scala"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Scala distribution: $PACKAGE ==="
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
    echo "Step 1: Generating main Scala modules..."
    "$HASKELL_BIN/transform-json-to-scala.sh" "$PACKAGE" main \
        --output "$DIST_ROOT"
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
        echo "Step 2: Generating test Scala modules..."
        "$HASKELL_BIN/transform-json-to-scala.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
# (testGraph.scala emptyGraph-to-buildTestGraph patch eliminated: the DSL now
# emits `hydra.test.testEnv.testGraph(testTypes)` directly, and the hand-written
# heads/scala testEnv.scala resolves the call to TestSuiteRunner.buildTestGraph.)
case "$PACKAGE" in
    hydra-kernel)
        # Copy hand-written hydra/lib/ from heads/scala into dist/.
        # heads/scala is the source of truth for these helpers; dist mirrors it
        # so that downstream consumers importing from dist/scala/hydra-kernel
        # see the same surface as the bootstrap demo.
        for d in lib; do
            LIB_SRC="$HYDRA_SCALA_HEAD/src/main/scala/hydra/$d"
            LIB_DST="$OUT_MAIN/hydra/$d"
            if [ -d "$LIB_SRC" ]; then
                echo "Step 3a: Copying hand-written hydra/$d/ from heads/scala/..."
                mkdir -p "$LIB_DST"
                cp -R "$LIB_SRC/." "$LIB_DST/"
            fi
        done
        ;;
    *)
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
