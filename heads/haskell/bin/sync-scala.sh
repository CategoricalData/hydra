#!/usr/bin/env bash
set -euo pipefail

# Script to synchronize Hydra-Scala with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Scala artifacts from the Hydra sources:
#   1. Build hydra-ext (compiles the Scala coder and test generator)
#   2. Generate Scala source modules
#   3. Generate Scala test modules
#   4. Compile Scala (unless --quick)
#
# Prerequisites:
#   - Hydra-Haskell must be built and up to date (run sync-haskell.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-scala.sh          # Full sync (all steps)
#   ./bin/sync-scala.sh --quick  # Skip Scala compilation

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/../.." && pwd )"
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
            echo "Synchronize Hydra-Scala with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip Scala compilation after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build hydra-ext"
            echo "  2. Generate Scala source modules"
            echo "  3. Generate Scala test modules"
            echo "  4. Compile and test Scala (unless --quick)"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

banner2 "Synchronizing Hydra-Scala"
echo ""

cd "$HYDRA_EXT_DIR"

TOTAL_STEPS=4

step 1 $TOTAL_STEPS "Building hydra-ext"
echo ""
stack build

step 2 $TOTAL_STEPS "Generating Scala source modules"
echo ""
stack exec update-scala -- $RTS_FLAGS

step 3 $TOTAL_STEPS "Generating Scala test modules"
echo ""
stack exec update-scala-tests -- $RTS_FLAGS

# Post-process generated test files
if [ -d "$DIST_SCALA/src/test/scala" ]; then
    # Replace unresolved inference type variables (T0-T99) with Any.
    # These appear in type parameter positions like [T0], [Int, T1], [T2, String].
    echo "  Post-processing: replacing inference type variables with Any..."
    find "$DIST_SCALA/src/test/scala" -name "*.scala" -exec \
        perl -pi -e 's/\bT(\d+)\b/Any/g' {} +
fi

# Patch testGraph.scala to use a graph populated with primitives instead of emptyGraph.
# Without this, evaluation tests produce "<<eval error>>" because no primitives are registered.
TESTGRAPH_FILE="$DIST_SCALA/src/test/scala/hydra/test/testGraph.scala"
if [ -f "$TESTGRAPH_FILE" ]; then
    echo "  Post-processing: patching testGraph.scala to use buildTestGraph..."
    sed_inplace 's/hydra\.lexical\.emptyGraph/hydra.TestSuiteRunner.buildTestGraph()/g' "$TESTGRAPH_FILE"
fi

if [ "$QUICK_MODE" = false ]; then
    step 4 $TOTAL_STEPS "Compiling and testing Scala"
    echo ""
    cd "$HYDRA_SCALA_DIR"
    sbt test
    cd "$HYDRA_EXT_DIR"
else
    step 4 $TOTAL_STEPS "Skipped (--quick mode)"
fi

banner2_done "Hydra-Scala sync complete!"
