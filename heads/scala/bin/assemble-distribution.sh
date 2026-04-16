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

echo "=== Assembling Scala distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Step 1: Main modules.
echo "Step 1: Generating main Scala modules..."
"$HASKELL_BIN/transform-json-to-scala.sh" "$PACKAGE" main \
    --output "$OUT_DIR/src/main"

# Step 2: Test modules.
echo ""
echo "Step 2: Generating test Scala modules..."
"$HASKELL_BIN/transform-json-to-scala.sh" "$PACKAGE" test \
    --output "$OUT_DIR/src/test"

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Patch testGraph.scala: replace emptyGraph with populated test graph.
        TESTGRAPH="$OUT_DIR/src/test/scala/hydra/test/testGraph.scala"
        if [ -f "$TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching testGraph.scala..."
            sed -i.bak 's/hydra\.lexical\.emptyGraph/hydra.TestSuiteRunner.buildTestGraph()/g' "$TESTGRAPH"
            rm -f "$TESTGRAPH.bak"
        fi
        ;;
    *)
        ;;
esac

echo ""
echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="
