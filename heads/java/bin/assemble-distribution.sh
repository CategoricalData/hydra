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

echo "=== Assembling Java distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Step 1: Main modules via Layer 1 transform.
# bootstrap-from-json appends <pkg>/src/main/<target> to --output, so we
# pass the dist-root directory (parent of per-package dirs).
echo "Step 1: Generating main Java modules..."
"$HASKELL_BIN/transform-json-to-java.sh" "$PACKAGE" main \
    --output "$DIST_ROOT" --include-dsls

# Step 2: Test modules.
echo ""
echo "Step 2: Generating test Java modules..."
"$HASKELL_BIN/transform-json-to-java.sh" "$PACKAGE" test \
    --output "$DIST_ROOT"

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Patch TestGraph.java: replace empty graph/context with TestEnv versions.
        TESTGRAPH="$OUT_DIR/src/test/java/hydra/test/TestGraph.java"
        if [ -f "$TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching TestGraph.java..."
            sed -i.bak 's/return hydra.Lexical.emptyGraph();/return hydra.test.TestEnv.testGraph();/' "$TESTGRAPH"
            sed -i.bak 's/return hydra.Lexical.emptyContext();/return hydra.test.TestEnv.testContext();/' "$TESTGRAPH"
            rm -f "$TESTGRAPH.bak"
        fi
        ;;
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
