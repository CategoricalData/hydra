#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Common Lisp distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/common-lisp/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_CL_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_CL_HEAD/../../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/common-lisp"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/common-lisp"
OUT_TEST="$OUT_DIR/src/test/common-lisp"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Common Lisp distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Common Lisp modules..."
    "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp main \
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
        echo "Step 2: Generating test Common Lisp modules..."
        "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

case "$PACKAGE" in
    hydra-kernel)
        # (Common Lisp exception: do NOT copy heads/lisp/common-lisp/.../lib/
        # into dist/common-lisp/hydra-kernel/. The Common Lisp test runner
        # loads from both heads and dist, so a copy could produce conflicting
        # symbol definitions at load time.)

        # (test_graph.lisp post-generation patch eliminated: the DSL
        # emits (cl:defvar hydra_test_test_graph_test_graph
        # (hydra_test_test_env_test_graph hydra_test_test_graph_test_types))
        # directly, and the hand-written
        # heads/lisp/common-lisp/.../test_env.lisp resolves the call.
        # run-tests.lisp loads test_env.lisp before test_graph.lisp.)
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
