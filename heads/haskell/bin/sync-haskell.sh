#!/usr/bin/env bash
set -euo pipefail

# Script to regenerate all Haskell artifacts for hydra-kernel and hydra-haskell
# from Hydra sources, via the two-stage DSL → JSON → Haskell pipeline.
#
# Stage 1 — DSL → JSON:
#   Runs the kernel DSL through the Haskell head and exports the complete
#   kernel universe (types, terms, eval lib, DSL modules, tests, and the
#   hydra.dsls aggregator) to dist/json/hydra-kernel/src/{main,test}/json/.
#
# Stage 2 — JSON → Haskell:
#   Runs bootstrap-from-json with --package-split, so that each loaded module
#   is routed to dist/haskell/<package>/ based on its namespace prefix.
#   Decoder/encoder source modules (Hydra.Sources.{Decode,Encode}.*) are
#   synthesized from the loaded kernel type modules via --synthesize-sources.
#
# This replaces the older multi-pass approach (update-haskell-kernel →
# update-kernel-tests → update-haskell-eval-lib → update-haskell-sources →
# update-haskell-kernel again) with a single JSON-reading generator call.
#
# Prerequisites:
#   - Stack is installed and configured
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
            echo "Regenerate hydra-kernel and hydra-haskell Haskell dist via DSL → JSON → Haskell."
            echo ""
            echo "Options:"
            echo "  --quick    Skip running tests after generation"
            echo "  --help     Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build required executables"
            echo "  2. Export kernel + test modules to JSON (DSL → JSON)"
            echo "  3. Verify JSON kernel + write manifest"
            echo "  4. Generate Haskell from JSON (JSON → Haskell)"
            echo "  5. Post-process generated files (TestGraph.hs patch)"
            echo "  6. Run tests (unless --quick)"
            echo "  7. Regenerate the lexicon"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

cd "$HYDRA_HASKELL_DIR"

TOTAL_STEPS=7

banner2 "Synchronizing Hydra-Haskell (via DSL → JSON → Haskell)"
echo ""

step 1 $TOTAL_STEPS "Building required executables"
echo ""
stack build \
    hydra:exe:update-json-main \
    hydra:exe:update-json-test \
    hydra:exe:update-json-manifest \
    hydra:exe:verify-json-kernel \
    hydra:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Exporting kernel + test modules to JSON"
echo ""
stack exec update-json-main -- $RTS_FLAGS
stack exec update-json-test -- $RTS_FLAGS

step 3 $TOTAL_STEPS "Verifying JSON kernel and writing manifest"
echo ""
stack exec verify-json-kernel -- $RTS_FLAGS
stack exec update-json-manifest

step 4 $TOTAL_STEPS "Generating Haskell from JSON"
echo ""
stack exec bootstrap-from-json -- \
    --target haskell \
    --output "../../dist/haskell" \
    --package-split \
    --include-dsls \
    --include-tests \
    --synthesize-sources \
    $RTS_FLAGS

step 5 $TOTAL_STEPS "Post-processing generated files"
echo ""

# Patch TestGraph.hs to use TestEnv (real graph with primitives) instead of emptyGraph.
# Without this, evaluation tests produce "<<eval error>>" because no primitives are registered.
#
# Note: Hydra.Test.TestEnv is a hand-written bridge module that lives under
# dist/haskell/hydra-kernel/src/test/haskell/Hydra/Test/TestEnv.hs and is
# checked into git. It is NOT generated; bootstrap-from-json doesn't target
# it (it's not in manifest.json's testModules), so it survives regeneration.
TESTGRAPH="../../dist/haskell/hydra-kernel/src/test/haskell/Hydra/Test/TestGraph.hs"
if [ -f "$TESTGRAPH" ]; then
    echo "  Patching TestGraph.hs..."
    sed_inplace 's/import qualified Hydra.Lexical as Lexical$/import qualified Hydra.Lexical as Lexical\nimport qualified Hydra.Test.TestEnv as TestEnv/' "$TESTGRAPH"
    sed_inplace 's/testGraph = Lexical.emptyGraph/testGraph = TestEnv.testGraph testTypes/' "$TESTGRAPH"
    sed_inplace 's/testContext = Lexical.emptyContext/testContext = TestEnv.testContext/' "$TESTGRAPH"
fi

# Rebuild so subsequent steps (test, lexicon) pick up the new Haskell dist.
echo ""
echo "  Rebuilding..."
stack build

if [ "$QUICK_MODE" = false ]; then
    step 6 $TOTAL_STEPS "Running tests"
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
    step 6 $TOTAL_STEPS "Skipped (--quick mode)"
fi

step 7 $TOTAL_STEPS "Regenerating lexicon"
echo ""
stack ghci hydra:lib --ghci-options='-e ":m Hydra.Haskell.Generation" -e "writeLexiconToStandardPath"' 2>&1 | grep -E "^Lexicon|^Error" || true

echo ""
echo "Checking for new files..."
echo ""

NEW_FILES=$(git status --porcelain ../../dist/haskell/hydra-kernel/src/main/haskell ../../dist/haskell/hydra-kernel/src/test/haskell ../../dist/json/hydra-kernel/src/main/json ../../dist/haskell/hydra-haskell/src/main/haskell 2>/dev/null | grep "^??" | awk '{print $2}' || true)

if [ -n "$NEW_FILES" ]; then
    echo "New files were created. You may want to run:"
    echo ""
    echo "  cd $HYDRA_HASKELL_DIR"
    echo "  git add ../../dist/haskell/hydra-kernel/src/main/haskell ../../dist/haskell/hydra-kernel/src/test/haskell ../../dist/json/hydra-kernel/src/main/json ../../dist/haskell/hydra-haskell/src/main/haskell"
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
