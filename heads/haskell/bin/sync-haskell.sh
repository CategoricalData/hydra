#!/usr/bin/env bash
set -euo pipefail

# Script to regenerate all Haskell artifacts for hydra-kernel and hydra-haskell
# from Hydra sources, via the two-stage DSL → JSON → Haskell pipeline.
#
# Stage 1 — DSL → JSON:
#   Runs the kernel DSL through the Haskell head and exports the universe
#   of Hydra modules (kernel + haskell coder + java/python/scala/lisp coders
#   + pg + rdf) to dist/json/<pkg>/src/main/json/ via the per-package
#   routing table in Hydra.PackageRouting. Test modules go to
#   dist/json/hydra-kernel/src/test/json/.
#
# Stage 2 — JSON → Haskell:
#   Runs bootstrap-from-json; each loaded module is routed to
#   dist/haskell/<package>/ based on its namespace prefix (via PackageRouting).
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
#   ./bin/sync-haskell.sh             # Full sync (all steps including tests)
#   ./bin/sync-haskell.sh --no-tests  # Skip tests (for faster iteration)
#   ./bin/sync-haskell.sh --help      # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

NO_TESTS=false

for arg in "$@"; do
    case $arg in
        --no-tests)
            NO_TESTS=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Regenerate hydra-kernel and hydra-haskell Haskell dist via DSL → JSON → Haskell."
            echo ""
            echo "Options:"
            echo "  --no-tests  Skip running tests after generation"
            echo "  --help      Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build required executables"
            echo "  2. Export kernel + test modules to JSON (DSL → JSON)"
            echo "  3. Verify JSON kernel + write manifest"
            echo "  4. Generate Haskell from JSON (JSON → Haskell)"
            echo "  5. Post-process generated files (TestGraph.hs patch)"
            echo "  6. Run tests (unless --no-tests)"
            echo "  7. Regenerate the lexicon"
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

cd "$HYDRA_HASKELL_DIR"

TOTAL_STEPS=6

banner2 "Synchronizing Hydra-Haskell (via DSL → JSON → Haskell)"
echo ""

step 1 $TOTAL_STEPS "Building required executables"
echo ""
stack build \
    hydra:exe:update-json-main \
    hydra:exe:update-json-test \
    hydra:exe:update-json-manifest \
    hydra:exe:verify-json-kernel \
    hydra:exe:bootstrap-from-json \
    hydra:exe:digest-check

step 2 $TOTAL_STEPS "Exporting kernel + test modules to JSON"
echo ""
stack exec update-json-main -- $RTS_FLAGS
stack exec update-json-test -- $RTS_FLAGS

step 3 $TOTAL_STEPS "Verifying JSON kernel and writing manifest"
echo ""

# Warm-cache short-circuit for verify-json-kernel: skip when no .json
# under dist/json/hydra-kernel/ has changed since the last green verify.
# verify-json-kernel is pure (parses each kernel JSON module and compares
# against the in-memory kernel), so the verdict only depends on the JSON
# inputs and the verifier binary. Per-module caching is the proper fix
# (see follow-up task) — this is the coarse interim.
VERIFY_CACHE="$HYDRA_HASKELL_DIR/.stack-work/verify-json-kernel-cache.txt"
compute_verify_hash() {
    {
        find "$HYDRA_ROOT_DIR/dist/json/hydra-kernel" -type f -name '*.json' 2>/dev/null
        # Include the exec source so a verifier change re-triggers verification.
        find "$HYDRA_ROOT_DIR/heads/haskell/src/exec/verify-json-kernel" -type f 2>/dev/null
    } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
}

CURRENT_VERIFY_HASH=$(compute_verify_hash)
RECORDED_VERIFY_HASH=""
if [ -f "$VERIFY_CACHE" ]; then
    RECORDED_VERIFY_HASH=$(cat "$VERIFY_CACHE")
fi

if [ -n "$RECORDED_VERIFY_HASH" ] && [ "$CURRENT_VERIFY_HASH" = "$RECORDED_VERIFY_HASH" ]; then
    echo "  JSON kernel inputs unchanged since last green verify; skipping."
else
    stack exec verify-json-kernel -- $RTS_FLAGS
    mkdir -p "$(dirname "$VERIFY_CACHE")"
    echo "$CURRENT_VERIFY_HASH" > "$VERIFY_CACHE"
fi
stack exec update-json-manifest

step 4 $TOTAL_STEPS "Generating Haskell from JSON"
echo ""

# Warm-cache short-circuit for bootstrap-from-json: skip when the JSON
# inputs under dist/json/ AND the bootstrap-from-json source are unchanged
# since the last successful run. Step 4 is pure given those inputs (parses
# JSON modules, routes per package, writes Haskell sources), so a cache
# hit guarantees the Haskell output on disk is identical to what would
# be regenerated.
BFJ_CACHE="$HYDRA_HASKELL_DIR/.stack-work/bootstrap-from-json-cache.txt"
compute_bfj_hash() {
    {
        find "$HYDRA_ROOT_DIR/dist/json" -type f -name '*.json' 2>/dev/null
        find "$HYDRA_ROOT_DIR/heads/haskell/src/exec/bootstrap-from-json" -type f 2>/dev/null
    } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
}

CURRENT_BFJ_HASH=$(compute_bfj_hash)
RECORDED_BFJ_HASH=""
if [ -f "$BFJ_CACHE" ]; then
    RECORDED_BFJ_HASH=$(cat "$BFJ_CACHE")
fi

if [ -n "$RECORDED_BFJ_HASH" ] && [ "$CURRENT_BFJ_HASH" = "$RECORDED_BFJ_HASH" ]; then
    echo "  JSON inputs + bootstrap-from-json unchanged since last run; skipping."
else
    stack exec bootstrap-from-json -- \
        --target haskell \
        --all-packages \
        --output "../../dist/haskell" \
        --include-dsls \
        --include-tests \
        --synthesize-sources \
        $RTS_FLAGS
    mkdir -p "$(dirname "$BFJ_CACHE")"
    echo "$CURRENT_BFJ_HASH" > "$BFJ_CACHE"
fi

step 5 $TOTAL_STEPS "Post-processing generated files"
echo ""

# TestGraph post-generation patch has been eliminated: the DSL at
# packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/TestGraph.hs
# now emits `TestEnv.testGraph testTypes` and `TestEnv.testContext`
# directly, via the FQN stubs in Hydra.Sources.Test.TestEnv. See
# task #25 in the feature_290_packaging plan for the detailed design.
echo "  (No post-processing needed — generator emits TestEnv refs directly.)"

# Rebuild so subsequent steps (test, lexicon) pick up the new Haskell dist.
echo ""
echo "  Rebuilding..."
stack build

if [ "$NO_TESTS" = false ]; then
    step 6 $TOTAL_STEPS "Running tests"
    echo ""

    # Warm-cache short-circuit: skip stack test when the input set is
    # byte-identical to the last successful run. Inputs that affect the
    # kernel test suite outcome:
    #   - dist/haskell/hydra-kernel/src/{main,test}/haskell/**.hs (generated)
    #   - heads/haskell/src/main/haskell/**.hs (hand-written runtime)
    #   - heads/haskell/src/test/haskell/**.hs (hand-written test infra)
    #   - heads/haskell/{package.yaml,stack.yaml} (build config)
    HASKELL_TEST_CACHE="$HYDRA_HASKELL_DIR/.stack-work/haskell-test-cache.txt"
    compute_haskell_test_hash() {
        {
            find "$HYDRA_ROOT_DIR/dist/haskell/hydra-kernel/src/main/haskell" \
                 "$HYDRA_ROOT_DIR/dist/haskell/hydra-kernel/src/test/haskell" \
                 "$HYDRA_HASKELL_DIR/src/main/haskell" \
                 "$HYDRA_HASKELL_DIR/src/test/haskell" \
                 -type f -name '*.hs' 2>/dev/null
            echo "$HYDRA_HASKELL_DIR/package.yaml"
            echo "$HYDRA_HASKELL_DIR/stack.yaml"
            echo "${BASH_SOURCE[0]}"
        } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
    }

    CURRENT_TEST_HASH=$(compute_haskell_test_hash)
    RECORDED_TEST_HASH=""
    if [ -f "$HASKELL_TEST_CACHE" ]; then
        RECORDED_TEST_HASH=$(cat "$HASKELL_TEST_CACHE")
    fi

    if [ -n "$RECORDED_TEST_HASH" ] && [ "$CURRENT_TEST_HASH" = "$RECORDED_TEST_HASH" ]; then
        echo "  Test inputs unchanged since last green run; skipping stack test."
    else
        TEST_LOG="$HYDRA_HASKELL_DIR/test-output.log"
        stack test 2>&1 | tee "$TEST_LOG"
        TEST_RESULT=${PIPESTATUS[0]}

        if [ $TEST_RESULT -eq 0 ]; then
            echo ""
            echo "All tests passed!"
            mkdir -p "$(dirname "$HASKELL_TEST_CACHE")"
            echo "$CURRENT_TEST_HASH" > "$HASKELL_TEST_CACHE"
        else
            echo ""
            echo "ERROR: stack test exited $TEST_RESULT. See $TEST_LOG" >&2
            exit $TEST_RESULT
        fi
    fi
else
    step 6 $TOTAL_STEPS "Skipped (--no-tests)"
fi

echo ""
# Stamp the phase1 input cache so bin/sync.sh can short-circuit Phase 1
# entirely on the next run when no inputs have changed. Only stamp when
# we actually ran (and passed) tests — stamping after --no-tests would
# poison the cache by recording a state that hasn't been validated.
if [ "$NO_TESTS" = false ]; then
    PHASE1_FRESH_CHECK="$HYDRA_ROOT_DIR/bin/lib/check-phase1-fresh.py"
    if [ -x "$PHASE1_FRESH_CHECK" ]; then
        "$PHASE1_FRESH_CHECK" "$HYDRA_ROOT_DIR" --record \
            && echo "  Phase 1 input cache stamped." || true
    fi
fi

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
