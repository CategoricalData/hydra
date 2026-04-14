#!/usr/bin/env bash
set -euo pipefail

# Script to synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Java artifacts from the Hydra sources:
#   1. Main modules, eval lib, and coder modules (from JSON)
#   2. Kernel test modules (from JSON)
#   3. Generation tests (from Haskell DSL)
#   4. Ext Java modules (PG model, decoders, encoders, etc.) into hydra-ext
#
# Prerequisites:
#   - JSON modules must be up to date (run sync-haskell.sh and sync-ext.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-java.sh          # Full sync (all steps)
#   ./bin/sync-java.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-java.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/../.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT_DIR/packages/hydra-java"

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
            echo "Synchronize Hydra-Java with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running Java tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build executable"
            echo "  2. Generate Java main modules and tests from JSON"
            echo "  3. Generate ext Java modules into hydra-ext"
            echo "  4. Generate ext Java modules into hydra-java"
            echo "  5. Build and test Java (unless --quick)"
            echo "  6. Report new files to git add"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

banner2 "Synchronizing Hydra-Java"
echo ""

check_native_jdk

cd "$HYDRA_EXT_DIR"

TOTAL_STEPS=5

step 1 $TOTAL_STEPS "Building executable"
echo ""
stack build hydra:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Generating Java main modules and tests from JSON"
echo ""
stack exec bootstrap-from-json -- --target java --include-coders --include-dsls --include-tests $RTS_FLAGS || \
    warn "Java test generation had errors (some polymorphic types not supported). Continuing..."

# Patch TestGraph.java to use TestEnv (real graph with primitives) instead of emptyGraph
TESTGRAPH="../../dist/java/hydra-kernel/src/test/java/hydra/test/TestGraph.java"
if [ -f "$TESTGRAPH" ]; then
    echo "  Post-processing: patching TestGraph.java..."
    sed_inplace 's/return hydra.Lexical.emptyGraph();/return hydra.test.TestEnv.testGraph();/' "$TESTGRAPH"
    sed_inplace 's/return hydra.Lexical.emptyContext();/return hydra.test.TestEnv.testContext();/' "$TESTGRAPH"
fi

step 3 $TOTAL_STEPS "Generating ext Java modules into dist/java/hydra-ext from JSON"
echo ""
stack exec bootstrap-from-json -- --target java --output "../../dist/java/hydra-ext" --include-coders --ext-only $RTS_FLAGS

# Patch Lisp Coder.java for PartialVisitor type inference issue in encodeTermDefinition
LISPCODER="../../dist/java/hydra-ext/src/main/java/hydra/lisp/Coder.java"
if [ -f "$LISPCODER" ]; then
    echo "  Post-processing: patching Lisp Coder.java..."
    sed_inplace 's/Either<hydra.lisp.syntax.TopLevelFormWithComments, hydra.lisp.syntax.TopLevelFormWithComments> otherwise/Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> otherwise/' "$LISPCODER"
    sed_inplace 's/Either<hydra.lisp.syntax.TopLevelFormWithComments, hydra.lisp.syntax.TopLevelFormWithComments> visit/Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> visit/' "$LISPCODER"
fi

if [ "$QUICK_MODE" = false ]; then
    step 4 $TOTAL_STEPS "Building and testing Java"
    echo ""

    cd "$HYDRA_ROOT_DIR"

    ./gradlew :hydra-java:compileJava
    ./gradlew :hydra-java:compileTestJava || \
        warn "Java test compilation had errors. Continuing..."
    ./gradlew :hydra-java:test || \
        warn "Some Java tests failed. Continuing..."

    cd "$HYDRA_EXT_DIR"
else
    step 4 $TOTAL_STEPS "Skipped (--quick mode)"
fi

step 5 $TOTAL_STEPS "Checking for new files"
echo ""

for CHECK_DIR in "$HYDRA_ROOT_DIR/dist/java/hydra-kernel" "$HYDRA_ROOT_DIR/dist/java/hydra-ext"; do
    cd "$CHECK_DIR" 2>/dev/null || continue
    LABEL=$(basename "$CHECK_DIR")

    NEW_FILES=$(git status --porcelain src/main/java src/test/java 2>/dev/null | grep "^??" | awk '{print $2}' || true)

    if [ -n "$NEW_FILES" ]; then
        echo "New files in $LABEL. You may want to run:"
        echo ""
        echo "  cd $CHECK_DIR"
        echo "  git add src/main/java src/test/java"
        echo ""
        echo "New files:"
        echo "$NEW_FILES" | head -20
        NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
        if [ "$NEW_COUNT" -gt 20 ]; then
            echo "  ... and $((NEW_COUNT - 20)) more"
        fi
        echo ""
    else
        echo "No new files in $LABEL."
    fi
done

banner2_done "Hydra-Java sync complete!"
