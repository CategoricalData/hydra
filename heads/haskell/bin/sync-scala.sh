#!/usr/bin/env bash
set -euo pipefail

# Script to regenerate hydra-kernel Scala dist from JSON modules.
#
# This script reads the kernel JSON sources (produced by sync-haskell.sh) and
# invokes `bootstrap-from-json --target scala`, which loads main + coder + test
# modules from JSON and emits Scala source files via the same Hydra.Scala.Coder
# used by the DSL-direct path. Long-line wrapping (Hydra.ExtGeneration.
# wrapLongLinesInScalaTree) runs as a post-pass inside bootstrap-from-json.
#
# This replaces the older two-pass approach
# (update-scala -> update-scala-tests).
#
# Prerequisites:
#   - Hydra-Haskell must be built and the JSON kernel up to date
#     (run sync-haskell.sh first)
#
# Usage:
#   ./bin/sync-scala.sh          # Full sync (all steps)
#   ./bin/sync-scala.sh --quick  # Skip Scala compilation

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"
HYDRA_SCALA_DIR="$HYDRA_ROOT_DIR/packages/hydra-scala"
DIST_SCALA="$HYDRA_ROOT_DIR/dist/scala/hydra-kernel"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

QUICK_MODE=false

for arg in "$@"; do
    case $arg in
        --quick)
            QUICK_MODE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Regenerate the hydra-kernel Scala dist from JSON kernel modules."
            echo ""
            echo "Options:"
            echo "  --quick    Skip Scala compilation after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build bootstrap-from-json"
            echo "  2. Generate Scala main + test modules from JSON"
            echo "  3. Post-process testGraph.scala"
            echo "  4. Compile and test Scala (unless --quick)"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

banner2 "Synchronizing Hydra-Scala (via JSON -> Scala)"
echo ""

cd "$HYDRA_HASKELL_DIR"

TOTAL_STEPS=4

step 1 $TOTAL_STEPS "Building bootstrap-from-json"
echo ""
stack build hydra:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Generating Scala from JSON"
echo ""
stack exec bootstrap-from-json -- \
    --target scala \
    --include-coders \
    --include-tests \
    $RTS_FLAGS

step 3 $TOTAL_STEPS "Post-processing generated files"
echo ""

# Patch testGraph.scala to use a graph populated with primitives instead of emptyGraph.
# Without this, evaluation tests produce "<<eval error>>" because no primitives are registered.
TESTGRAPH_FILE="$DIST_SCALA/src/test/scala/hydra/test/testGraph.scala"
if [ -f "$TESTGRAPH_FILE" ]; then
    echo "  Patching testGraph.scala to use buildTestGraph..."
    sed_inplace 's/hydra\.lexical\.emptyGraph/hydra.TestSuiteRunner.buildTestGraph()/g' "$TESTGRAPH_FILE"
fi

if [ "$QUICK_MODE" = false ]; then
    step 4 $TOTAL_STEPS "Compiling and testing Scala"
    echo ""
    cd "$HYDRA_SCALA_DIR"
    sbt test
    cd "$HYDRA_HASKELL_DIR"
else
    step 4 $TOTAL_STEPS "Skipped (--quick mode)"
fi

banner2_done "Hydra-Scala sync complete!"
