#!/usr/bin/env bash
# Regenerate all Haskell artifacts for hydra-kernel and hydra-haskell from
# Hydra sources, via the two-stage DSL → JSON → Haskell pipeline.
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
# update-kernel-tests → update-haskell-default-lib → update-haskell-sources →
# update-haskell-kernel again) with a single JSON-reading generator call.
#
# Steps performed:
#   1. Build required executables
#   2. Export kernel + test modules to JSON (DSL → JSON)
#   3. Verify JSON kernel (reconcile drift via update-json-kernel) + write manifest
#   4. Generate Haskell from JSON (JSON → Haskell)
#   5. Post-process generated files (no-op since #307)
#   6. Run tests (unless --no-tests)
#
# The lexicon (docs/hydra-lexicon.txt) is no longer regenerated here;
# refresh it on demand with bin/regenerate-lexicon.sh (or /lexicon()).
#
# Prerequisites:
#   - Stack is installed and configured
#
# Usage:
#   heads/haskell/bin/sync-haskell.sh             # Full sync (all steps including tests)
#   heads/haskell/bin/sync-haskell.sh --no-tests  # Skip tests (for faster iteration)
#   heads/haskell/bin/sync-haskell.sh --help      # Show this help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

NO_TESTS=false

while [ $# -gt 0 ]; do
    case "$1" in
        --no-tests)
            NO_TESTS=true
            ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            die "Unknown argument: $1 (try --help)"
            ;;
    esac
    shift
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
    hydra:exe:update-json-kernel \
    hydra:exe:verify-json-kernel \
    hydra:exe:bootstrap-from-json \
    hydra:exe:digest-check

step 2 $TOTAL_STEPS "Exporting kernel + test modules to JSON"
echo ""
# #344: by default, the legacy Haskell DSL skips hydra.java.* / hydra.python.*
# JSON (the native generators own those paths). On a cold tree where their
# JSON is missing, set HYDRA_INCLUDE_JAVA_PYTHON=1 (sync.sh does this
# automatically) to seed the bootstrap with one Haskell-DSL pass.
JP_FLAG=""
if [ "${HYDRA_INCLUDE_JAVA_PYTHON:-0}" = "1" ]; then
    JP_FLAG="--include-java-python"
    echo "  (cold-start bootstrap: --include-java-python set; legacy Haskell DSL"
    echo "   will write hydra.java.*/hydra.python.* JSON this once; see #344)"
fi
stack exec update-json-main -- $JP_FLAG $RTS_FLAGS
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
VERIFY_HASH=$(
    {
        find "$HYDRA_ROOT_DIR/dist/json/hydra-kernel" -type f -name '*.json' 2>/dev/null
        # Include the exec source so a verifier change re-triggers verification.
        find "$HYDRA_ROOT_DIR/heads/haskell/src/exec/verify-json-kernel" -type f 2>/dev/null
    } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
)

if step_cache_hit "$VERIFY_CACHE" "$VERIFY_HASH"; then
    echo "  JSON kernel inputs unchanged since last green verify; skipping."
else
    # Verify-with-reconcile (#392). verify-json-kernel decodes each committed
    # dist/json/hydra-kernel/.../<module>.json and compares it against what the
    # source DSL (kernelModules) produces. On drift it exits 1 with e.g.
    # "element count differs: N vs M". The recovery is purely mechanical: run
    # update-json-kernel — the authoritative DSL→JSON writer for exactly this
    # tree (kernelModules ++ defaultLibModules → dist/json/hydra-kernel/.../) —
    # then re-verify. So do that automatically instead of forcing the human to.
    #
    # This is NOT a post-generation patch: it runs the generator to make
    # dist/json match the source DSL, then re-verifies and still fails hard if
    # drift persists (which would mean a real, non-deterministic generator bug).
    # It mirrors the freshness-gate reconciliation pattern of #387 / f368f8220a.
    if stack exec verify-json-kernel -- $RTS_FLAGS; then
        step_cache_record "$VERIFY_CACHE" "$VERIFY_HASH"
    else
        echo ""
        echo "  Verify reported JSON kernel drift; reconciling via update-json-kernel..."
        echo ""
        stack exec update-json-kernel -- $RTS_FLAGS
        echo ""
        echo "  Regenerated kernel JSON (drift reconciled). Changed files:"
        git -C "$HYDRA_ROOT_DIR" diff --stat -- dist/json/hydra-kernel || true
        echo ""
        echo "  Re-verifying..."
        echo ""
        if stack exec verify-json-kernel -- $RTS_FLAGS; then
            # Re-verify passed: recompute the cache hash over the now-regenerated
            # JSON so the green verdict is recorded against the current inputs.
            VERIFY_HASH=$(
                {
                    find "$HYDRA_ROOT_DIR/dist/json/hydra-kernel" -type f -name '*.json' 2>/dev/null
                    find "$HYDRA_ROOT_DIR/heads/haskell/src/exec/verify-json-kernel" -type f 2>/dev/null
                } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
            )
            step_cache_record "$VERIFY_CACHE" "$VERIFY_HASH"
            echo "  Reconciled. The regenerated dist/json is left in your working"
            echo "  tree for review/commit."
        else
            die "verify-json-kernel still reports drift after update-json-kernel regenerated the JSON. This is a real generator bug (non-deterministic DSL→JSON output), not reconcilable drift — investigate the kernel JSON generator before re-running."
        fi
    fi
fi
stack exec update-json-manifest

step 4 $TOTAL_STEPS "Generating Haskell from JSON"
echo ""

# Warm-cache short-circuit for bootstrap-from-json: skip when the JSON
# inputs under dist/json/ AND every hand-written source bootstrap-from-json
# can transitively use to produce its output are unchanged since the last
# successful run. Step 4 is pure given those inputs (parses JSON modules,
# routes per package, writes Haskell sources), so a cache hit guarantees
# the on-disk output is identical to what would be regenerated.
#
# Sources hashed:
#   * dist/json/**/*.json — the module inputs
#   * heads/haskell/src/exec/bootstrap-from-json/** — the driver
#   * heads/haskell/src/main/haskell/**.hs — hand-written runtime,
#     including Hydra.Generation, Hydra.Haskell.Generation,
#     Hydra.PackageRouting, etc. Editing any of these can change what
#     the binary emits without changing JSON inputs or the driver source;
#     omitting them left dist/haskell/ stale until a forced rebuild.
#     Matches the scope of bin/lib/check-phase1-fresh.py for consistency.
#     The right end-state is a per-transform Merkle hash of the exec's
#     transitive import closure — see #347.
BFJ_CACHE="$HYDRA_HASKELL_DIR/.stack-work/bootstrap-from-json-cache.txt"
BFJ_HASH=$(
    {
        find "$HYDRA_ROOT_DIR/dist/json" -type f -name '*.json' 2>/dev/null
        find "$HYDRA_ROOT_DIR/heads/haskell/src/exec/bootstrap-from-json" -type f 2>/dev/null
        find "$HYDRA_ROOT_DIR/heads/haskell/src/main/haskell" -type f -name '*.hs' 2>/dev/null
    } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
)

if step_cache_hit "$BFJ_CACHE" "$BFJ_HASH"; then
    echo "  JSON inputs + bootstrap-from-json unchanged since last run; skipping."
else
    stack exec bootstrap-from-json -- \
        --target haskell \
        --all-packages \
        --output "../../dist/haskell" \
        --include-dsls \
        --include-tests \
        --synthesize-sources \
        --prune-stale \
        $RTS_FLAGS
    step_cache_record "$BFJ_CACHE" "$BFJ_HASH"
fi

step 5 $TOTAL_STEPS "Post-processing generated files"
echo ""

# All post-generation patches have been eliminated as of #307. The DSL
# at packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/TestGraph.hs
# emits `TestEnv.testGraph testTypes testTerms` and `TestEnv.testContext`
# directly via the FQN stubs in Hydra.Sources.Test.TestEnv; the Scala
# line-wrap is now applied during emission via
# generateSourcesWithTransform; the EL/CL/Clojure/Scheme test_graph
# files reference test_env.* directly with no rewriting needed.
echo "  (No post-processing needed — all patches eliminated, see #307.)"

# Overlay hand-written distribution-package source onto the dist tree (#418) so
# each dist/haskell/<pkg>/ is a COMPLETE, self-contained distribution package (the
# source-package -> complete-target-distribution model; see docs/build-system.md).
# This is the Haskell analog of Java/Python's copy-kernel-runtime.sh. The overlay
# trees are copies, not patches: they add package source under dist/, they do not
# edit any generated file. Canonical homes are the uncompiled trees under the
# top-level overlay/haskell/ directory (a sibling of dist/, packages/, heads/).
#
# IMPORTANT: the head compiles dist/haskell/hydra-kernel/ (a source-dir in
# package.yaml) but does NOT compile overlay/haskell/hydra-kernel/. So overlaying
# the runtime onto dist/haskell/hydra-kernel/ here is what puts those modules on
# the head's compile path — exactly once, from the dist copy. (Adding the overlay
# to the head's source-dirs as well would double-compile them; package.yaml does
# not.)
echo ""
# Overlay hand-written runtime from overlay/haskell/ onto dist/haskell/. Extracted
# into a standalone script (#418 fix) so bin/sync.sh can run the same overlay BEFORE
# its Phase 0 stack build — sync-haskell.sh's own invocation here was too late for
# that earlier build. Idempotent (cp -R), so calling it in both places is safe.
"$SCRIPT_DIR/overlay-kernel-runtime.sh"

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
    #
    # $SCRIPT_DIR is absolute (captured before any cd); using
    # ${BASH_SOURCE[0]} relative would fail to resolve after the
    # script has cd'd into $HYDRA_HASKELL_DIR. Diagnosed by
    # feature_343_json on 2026-04-26.
    HASKELL_TEST_CACHE="$HYDRA_HASKELL_DIR/.stack-work/haskell-test-cache.txt"
    HASKELL_TEST_HASH=$(
        {
            find "$HYDRA_ROOT_DIR/dist/haskell/hydra-kernel/src/main/haskell" \
                 "$HYDRA_ROOT_DIR/dist/haskell/hydra-kernel/src/test/haskell" \
                 "$HYDRA_HASKELL_DIR/src/main/haskell" \
                 "$HYDRA_HASKELL_DIR/src/test/haskell" \
                 -type f -name '*.hs' 2>/dev/null
            echo "$HYDRA_HASKELL_DIR/package.yaml"
            echo "$HYDRA_HASKELL_DIR/stack.yaml"
            echo "$SCRIPT_DIR/sync-haskell.sh"
        } | LC_ALL=C sort | xargs shasum -a 256 2>/dev/null | shasum -a 256 | awk '{print $1}'
    )

    if step_cache_hit "$HASKELL_TEST_CACHE" "$HASKELL_TEST_HASH"; then
        echo "  Test inputs unchanged since last green run; skipping stack test."
    else
        TEST_LOG="$HYDRA_HASKELL_DIR/test-output.log"
        stack test 2>&1 | tee "$TEST_LOG"
        TEST_RESULT=${PIPESTATUS[0]}

        if [ $TEST_RESULT -eq 0 ]; then
            echo ""
            echo "All tests passed!"
            step_cache_record "$HASKELL_TEST_CACHE" "$HASKELL_TEST_HASH"
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
