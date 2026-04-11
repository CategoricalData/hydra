#!/usr/bin/env bash
set -euo pipefail

# Script to regenerate all Haskell artifacts from Hydra sources and run tests.
#
# This is the Haskell counterpart to the hydra-ext/bin/sync-*.sh scripts.
# It ensures hydra-haskell is in a fully consistent state by regenerating all
# generated Haskell code in the correct order, with rebuilds between phases
# (since each phase writes .hs files that subsequent phases depend on).
#
# Steps:
#   1. Generate kernel modules
#   2. Generate kernel test modules
#   3. Generate eval lib modules
#   4. Generate encoder/decoder source modules
#   5. Regenerate kernel modules (to pick up new encoder/decoder sources)
#   6. Export and verify JSON kernel
#   7. Run tests (unless --quick)
#
# Prerequisites:
#   - Stack is installed and configured
#   - Run from the hydra-haskell directory (or the script will cd there)
#
# Usage:
#   ./bin/sync-haskell.sh          # Full sync (all steps including tests)
#   ./bin/sync-haskell.sh --quick  # Skip tests (for faster iteration)
#   ./bin/sync-haskell.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

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
            echo "Regenerate all Haskell artifacts from Hydra sources."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Generate kernel modules"
            echo "  2. Generate kernel test modules"
            echo "  3. Generate eval lib modules"
            echo "  4. Generate encoder/decoder source modules"
            echo "  5. Regenerate kernel modules (picks up new encoder/decoder sources)"
            echo "  6. Export and verify JSON kernel"
            echo "  7. Run tests (unless --quick)"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

cd "$HYDRA_HASKELL_DIR"

TOTAL_STEPS=7

banner2 "Synchronizing Hydra-Haskell (from DSL)"
echo ""

step 1 $TOTAL_STEPS "Generating kernel modules"
echo ""
stack build hydra:exe:update-haskell-kernel
stack exec update-haskell-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel generation failed"
    exit 1
fi

echo ""
echo "  Rebuilding..."
stack build

step 2 $TOTAL_STEPS "Generating kernel test modules"
echo ""
stack build hydra:exe:update-kernel-tests
stack exec update-kernel-tests -- $RTS_FLAGS

# Patch TestGraph.hs to use TestEnv (real graph with primitives) instead of emptyGraph
TESTGRAPH="../hydra-kernel/src/gen-test/haskell/Hydra/Test/TestGraph.hs"
if [ -f "$TESTGRAPH" ]; then
    echo "  Post-processing: patching TestGraph.hs..."
    sed_inplace 's/import qualified Hydra.Lexical as Lexical$/import qualified Hydra.Lexical as Lexical\nimport qualified Hydra.Test.TestEnv as TestEnv/' "$TESTGRAPH"
    sed_inplace 's/testGraph = Lexical.emptyGraph/testGraph = TestEnv.testGraph testTypes/' "$TESTGRAPH"
    sed_inplace 's/testContext = Lexical.emptyContext/testContext = TestEnv.testContext/' "$TESTGRAPH"
fi

echo ""
echo "  Rebuilding..."
stack build

step 3 $TOTAL_STEPS "Generating eval lib modules"
echo ""
stack build hydra:exe:update-haskell-eval-lib
stack exec update-haskell-eval-lib -- $RTS_FLAGS

echo ""
echo "  Rebuilding..."
stack build

step 4 $TOTAL_STEPS "Generating encoder/decoder source modules"
echo ""
stack build hydra:exe:update-haskell-sources
stack exec update-haskell-sources -- $RTS_FLAGS

echo ""
echo "  Rebuilding..."
stack build

step 5 $TOTAL_STEPS "Regenerating kernel modules (post encoder/decoder)"
echo ""
stack exec update-haskell-kernel -- $RTS_FLAGS

if [ $? -ne 0 ]; then
    echo "ERROR: Kernel regeneration failed"
    exit 1
fi

echo ""
echo "  Rebuilding..."
stack build

step 6 $TOTAL_STEPS "Exporting and verifying JSON"
echo ""
stack build hydra:exe:update-json-main hydra:exe:update-json-test hydra:exe:verify-json-kernel hydra:exe:update-json-manifest
stack exec update-json-main -- $RTS_FLAGS
stack exec update-json-test -- $RTS_FLAGS
stack exec verify-json-kernel -- $RTS_FLAGS
stack exec update-json-manifest

if [ "$QUICK_MODE" = false ]; then
    step 7 $TOTAL_STEPS "Running tests"
    echo ""

    TEST_LOG="$HYDRA_HASKELL_DIR/test-output.log"
    stack test 2>&1 | tee "$TEST_LOG"
    TEST_RESULT=${PIPESTATUS[0]}

    if [ $TEST_RESULT -eq 0 ]; then
        echo ""
        echo "All tests passed!"
    else
        warn "Some tests failed (exit code $TEST_RESULT). See $TEST_LOG"
    fi
else
    step 7 $TOTAL_STEPS "Skipped (--quick mode)"
fi

# Regenerate the lexicon
echo ""
echo "  Regenerating lexicon..."
stack ghci hydra:lib --ghci-options='-e ":m Hydra.Haskell.Generation" -e "writeLexiconToStandardPath"' 2>&1 | grep -E "^Lexicon|^Error" || true

echo ""
echo "Checking for new files..."
echo ""

NEW_FILES=$(git status --porcelain src/gen-main/haskell ../hydra-kernel/src/gen-main/haskell ../hydra-kernel/src/gen-test/haskell ../hydra-kernel/src/gen-main/json 2>/dev/null | grep "^??" | awk '{print $2}' || true)

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_HASKELL_DIR"
    echo "  git add src/gen-main/haskell ../hydra-kernel/src/gen-main/haskell ../hydra-kernel/src/gen-test/haskell ../hydra-kernel/src/gen-main/json"
    echo ""
    echo "New files:"
    echo "$NEW_FILES" | head -20
    NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
    if [ "$NEW_COUNT" -gt 20 ]; then
        echo "  ... and $((NEW_COUNT - 20)) more"
    fi
else
    echo "No new files created."
fi

banner2_done "Hydra-Haskell sync complete!"
