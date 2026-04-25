#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Java distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../dist/java/<pkg>/) by:
#   1. Calling Layer 1 transform-json-to-java.sh for main modules
#   2. Calling Layer 1 transform-json-to-java.sh for test modules
#   3. Applying package-specific post-processing:
#      - hydra-kernel: patch TestGraph.java
#      - hydra-lisp:   patch Coder.java (PartialVisitor type inference)
#
# Assemblers do NOT run tests; see test-distribution.sh (Layer 2.5).
#
# Note: build.gradle files are tracked in-tree under packages/hydra-java/,
# not per-dist. Per-package build files are future work.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

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

OUT_DIR="$DIST_ROOT/$PACKAGE"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
OUT_MAIN_DIR="$OUT_DIR/src/main/java"
OUT_TEST_DIR="$OUT_DIR/src/test/java"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Java distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. The per-source-set
# digest pair (input at dist/json/<pkg>/src/<set>/digest.json, output at
# dist/<lang>/<pkg>/src/<set>/digest.json) is the single source of
# truth for "does this set need to regen?". Replaces the older split of
# per-package digest.json + .{main,test}-input-hash.txt dotfiles.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN_DIR" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Java modules..."
    "$HASKELL_BIN/transform-json-to-java.sh" "$PACKAGE" main \
        --output "$DIST_ROOT" --include-dsls
    assemble_refresh_digest "$INPUT_DIGEST_MAIN" "$OUT_MAIN_DIR" "$OUTPUT_DIGEST_MAIN"
fi

# Step 2: Test modules. Any package can have a test source set (just
# `dist/json/<pkg>/src/test/json/`); the per-source-set digest mechanism
# is uniform — adding a test dir for any package automatically wires it
# into the build.
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    if assemble_check_fresh "$INPUT_DIGEST_TEST" "$OUT_TEST_DIR" "$OUTPUT_DIGEST_TEST"; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        rm -f "$OUTPUT_DIGEST_TEST"
        echo "Step 2: Generating test Java modules..."
        "$HASKELL_BIN/transform-json-to-java.sh" "$PACKAGE" test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST_DIR" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
# (TestGraph.java post-generation patch eliminated: the DSL now emits
# TestEnv refs directly. See task #25 in feature_290_packaging plan.)
case "$PACKAGE" in
    hydra-lisp)
        # Patch Lisp Coder.java: fix PartialVisitor type inference in
        # encodeTermDefinition.
        LISPCODER="$OUT_DIR/src/main/java/hydra/lisp/Coder.java"
        if [ -f "$LISPCODER" ]; then
            echo ""
            echo "Step 3: Patching Lisp Coder.java..."
            sed -i.bak 's/Either<hydra.lisp.syntax.TopLevelFormWithComments, hydra.lisp.syntax.TopLevelFormWithComments> otherwise/Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> otherwise/' "$LISPCODER"
            sed -i.bak 's/Either<hydra.lisp.syntax.TopLevelFormWithComments, hydra.lisp.syntax.TopLevelFormWithComments> visit/Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> visit/' "$LISPCODER"
            rm -f "$LISPCODER.bak"
        fi
        ;;
    *)
        # No per-package post-processing for other packages today.
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
