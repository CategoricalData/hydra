#!/usr/bin/env bash
set -euo pipefail

# Top-level synchronization script for Hydra.
#
# Runs all generation and sync steps in the correct order:
#
#   Phase 1: Generate Haskell from DSL (hydra-haskell)
#     - Kernel modules, test modules, eval lib, encoder/decoder sources
#     - Regenerate kernel (picks up new sources)
#     - Export and verify JSON, generate manifest
#
#   Phase 2: Generate ext modules (hydra-ext)
#     - Ext Haskell modules (Java/Python coders, language syntaxes, etc.)
#     - Export ext modules to JSON
#
#   Phases 3+: Generate target languages from JSON (hydra-ext)
#     - Java, Python, Scala, Lisp (from JSON via bootstrap-from-json)
#     - Haskell is already fully synced by Phases 1-2
#
# Stops at the first error. Times the entire operation.
#
# Prerequisites:
#   - Stack is installed and configured
#   - Run from the repo root (or the script will cd there)
#
# Usage:
#   ./bin/sync-all.sh                                        # Default targets: hydra,java,python
#   ./bin/sync-all.sh --quick                                # Skip tests in each phase
#   ./bin/sync-all.sh --targets all                          # All implementations
#   ./bin/sync-all.sh --targets hydra,java,python,lisp       # Include Lisp dialects
#   ./bin/sync-all.sh --targets clojure,scheme               # Only specific Lisp dialects
#   ./bin/sync-all.sh --help                                 # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/heads/haskell"
HYDRA_EXT_DIR="$HYDRA_ROOT/packages/hydra-ext"

source "$HYDRA_ROOT/bin/lib/common.sh"

QUICK_MODE=false
TARGETS="hydra,java,python"

while [ $# -gt 0 ]; do
    case "$1" in
        --quick)
            QUICK_MODE=true
            ;;
        --targets)
            TARGETS="$2"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Run all Hydra sync steps in the correct order."
            echo ""
            echo "Options:"
            echo "  --quick              Skip tests in each phase"
            echo "  --targets T,...      Comma-separated list of targets (default: hydra,java,python)"
            echo "  --help               Show this help message"
            echo ""
            echo "Targets:"
            echo "  hydra       Haskell kernel regeneration"
            echo "  java        Java code generation"
            echo "  python      Python code generation"
            echo "  scala       Scala code generation"
            echo "  clojure     Clojure code generation"
            echo "  common-lisp Common Lisp code generation"
            echo "  emacs-lisp  Emacs Lisp code generation"
            echo "  scheme      Scheme code generation"
            echo "  lisp        All four Lisp dialects"
            echo "  all         All implementations"
            echo ""
            echo "Default phases (with --targets hydra,java,python):"
            echo "  1. Generate Haskell from DSL (kernel, tests, eval lib, sources, JSON)"
            echo "  2. Generate ext modules and JSON (hydra-ext)"
            echo "  3. Sync Java from JSON"
            echo "  4. Sync Python from JSON"
            echo ""
            echo "Stops at the first error. Reports total elapsed time."
            exit 0
            ;;
        *)
            die "Unknown argument: $1 (try --help)"
            ;;
    esac
    shift
done

# Expand special target names
case "$TARGETS" in
    all) TARGETS="hydra,java,python,scala,clojure,common-lisp,emacs-lisp,scheme" ;;
esac

# Parse targets into flags
TARGET_HYDRA=false
TARGET_JAVA=false
TARGET_PYTHON=false
TARGET_SCALA=false
LISP_DIALECTS=()

IFS=',' read -ra TARGET_LIST <<< "$TARGETS"
for t in "${TARGET_LIST[@]}"; do
    case "$t" in
        hydra)       TARGET_HYDRA=true ;;
        java)        TARGET_JAVA=true ;;
        python)      TARGET_PYTHON=true ;;
        scala)       TARGET_SCALA=true ;;
        lisp)        LISP_DIALECTS+=(clojure common-lisp emacs-lisp scheme) ;;
        clojure)     LISP_DIALECTS+=(clojure) ;;
        common-lisp) LISP_DIALECTS+=(common-lisp) ;;
        emacs-lisp)  LISP_DIALECTS+=(emacs-lisp) ;;
        scheme)      LISP_DIALECTS+=(scheme) ;;
        *)
            die "Unknown target '$t'. Valid targets: hydra, java, python, scala, lisp, clojure, common-lisp, emacs-lisp, scheme, all"
            ;;
    esac
done

# Deduplicate Lisp dialects
if [ ${#LISP_DIALECTS[@]} -gt 0 ]; then
    LISP_DIALECTS=($(printf '%s\n' "${LISP_DIALECTS[@]}" | sort -u))
fi

HAS_LISP=false
if [ ${#LISP_DIALECTS[@]} -gt 0 ]; then
    HAS_LISP=true
fi

# Phase 2 (ext) is needed if any target depends on it
NEED_EXT=false
if $TARGET_HYDRA || $TARGET_JAVA || $TARGET_PYTHON || $TARGET_SCALA || $HAS_LISP; then
    NEED_EXT=true
fi

# Ensure JAVA_HOME is set to a JDK 11+ (required for Gradle builds)
if $TARGET_JAVA; then
    if [ -z "${JAVA_HOME:-}" ]; then
        if command -v /usr/libexec/java_home &>/dev/null; then
            export JAVA_HOME="$(/usr/libexec/java_home 2>/dev/null || true)"
        fi
    fi
    if [ -z "${JAVA_HOME:-}" ]; then
        warn "JAVA_HOME is not set. Java compilation steps may fail."
        warn "Set JAVA_HOME to a JDK 11+ installation before running this script."
    else
        echo "Using JAVA_HOME=$JAVA_HOME"
    fi

    # Warn if running an x86_64 JDK under Rosetta on Apple Silicon (causes ~20x slowdown)
    if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
        JAVA_CMD="${JAVA_HOME:+$JAVA_HOME/bin/}java"
        if command -v "$JAVA_CMD" > /dev/null 2>&1 && file "$(command -v "$JAVA_CMD")" | grep -q x86_64; then
            warn "x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2 and will be ~20x slower than a native arm64 JDK."
            warn "Current JDK: $("$JAVA_CMD" -version 2>&1 | head -1)"
        fi
    fi
fi

START_TIME=$SECONDS

print_elapsed() {
    ELAPSED=$((SECONDS - START_TIME))
    MINUTES=$((ELAPSED / 60))
    SECS=$((ELAPSED % 60))
    echo ""
    echo "Total elapsed time: ${MINUTES}m ${SECS}s"
}

# Trap to print elapsed time on exit (success or failure)
trap print_elapsed EXIT

# Count total phases for display
TOTAL_PHASES=0
if $TARGET_HYDRA; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $NEED_EXT; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $TARGET_JAVA; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $TARGET_PYTHON; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $TARGET_SCALA; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $HAS_LISP; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
CURRENT_PHASE=0

banner1 "Hydra full sync (targets: $TARGETS)"
echo ""

QUICK_FLAG=""
if [ "$QUICK_MODE" = true ]; then
    QUICK_FLAG="--quick"
fi

phase_banner() {
    CURRENT_PHASE=$((CURRENT_PHASE + 1))
    echo ""
    banner1 "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: $1"
    echo ""
}

# ──────────────────────────────────────────────────
# Phase: Generate Haskell from DSL (hydra-haskell)
# ──────────────────────────────────────────────────

if $TARGET_HYDRA; then
    phase_banner "Generating Haskell from DSL"

    cd "$HYDRA_HASKELL_DIR"

    echo "Step 1a: Generating kernel modules..."
    echo ""
    stack build hydra:exe:update-haskell-kernel
    stack exec update-haskell-kernel -- $RTS_FLAGS
    echo ""
    echo "  Rebuilding..."
    stack build

    echo ""
    echo "Step 1b: Generating kernel test modules..."
    echo ""
    stack build hydra:exe:update-kernel-tests
    stack exec update-kernel-tests -- $RTS_FLAGS

    # Patch TestGraph.hs to use TestEnv (real graph with primitives) instead of emptyGraph
    TESTGRAPH="../../packages/hydra-kernel/src/gen-test/haskell/Hydra/Test/TestGraph.hs"
    if [ -f "$TESTGRAPH" ]; then
        echo "  Post-processing: patching TestGraph.hs..."
        sed_inplace 's/import qualified Hydra.Lexical as Lexical$/import qualified Hydra.Lexical as Lexical\nimport qualified Hydra.Test.TestEnv as TestEnv/' "$TESTGRAPH"
        sed_inplace 's/testGraph = Lexical.emptyGraph/testGraph = TestEnv.testGraph testTypes/' "$TESTGRAPH"
        sed_inplace 's/testContext = Lexical.emptyContext/testContext = TestEnv.testContext/' "$TESTGRAPH"
    fi

    echo ""
    echo "  Rebuilding..."
    stack build

    echo ""
    echo "Step 1c: Generating eval lib modules..."
    echo ""
    stack build hydra:exe:update-haskell-eval-lib
    stack exec update-haskell-eval-lib -- $RTS_FLAGS
    echo ""
    echo "  Rebuilding..."
    stack build

    echo ""
    echo "Step 1d: Generating encoder/decoder source modules..."
    echo ""
    stack build hydra:exe:update-haskell-sources
    stack exec update-haskell-sources -- $RTS_FLAGS
    echo ""
    echo "  Rebuilding..."
    stack build

    echo ""
    echo "Step 1e: Regenerating kernel modules (post encoder/decoder)..."
    echo ""
    stack exec update-haskell-kernel -- $RTS_FLAGS
    echo ""
    echo "  Rebuilding..."
    stack build

    if [ "$QUICK_MODE" = false ]; then
        echo ""
        echo "Step 1f: Running Haskell tests..."
        echo ""
        stack test 2>&1
    fi

    echo ""
    echo "Step 1g: Exporting and verifying JSON..."
    echo ""
    stack build hydra:exe:update-json-main hydra:exe:verify-json-kernel hydra:exe:update-json-test
    stack exec update-json-main -- $RTS_FLAGS
    stack exec update-json-test -- $RTS_FLAGS
    stack exec verify-json-kernel -- $RTS_FLAGS

    echo ""
    echo "Step 1h: Generating JSON manifest..."
    echo ""
    stack build hydra:exe:update-json-manifest
    stack exec update-json-manifest
fi

# ──────────────────────────────────────────────────
# Phase: Generate ext modules (hydra-ext)
# ──────────────────────────────────────────────────

if $NEED_EXT; then
    phase_banner "Synchronizing Hydra-Ext"
    "$HYDRA_EXT_DIR/bin/sync-ext.sh"
fi

# ──────────────────────────────────────────────────
# Phase: Sync Java from JSON
# ──────────────────────────────────────────────────

if $TARGET_JAVA; then
    phase_banner "Synchronizing Java (from JSON)"
    "$HYDRA_EXT_DIR/bin/sync-java.sh" $QUICK_FLAG
fi

# ──────────────────────────────────────────────────
# Phase: Sync Python from JSON
# ──────────────────────────────────────────────────

if $TARGET_PYTHON; then
    phase_banner "Synchronizing Python (from JSON)"
    "$HYDRA_EXT_DIR/bin/sync-python.sh" $QUICK_FLAG
fi

# ──────────────────────────────────────────────────
# Phase: Sync Scala
# ──────────────────────────────────────────────────

if $TARGET_SCALA; then
    phase_banner "Synchronizing Scala"
    "$HYDRA_EXT_DIR/bin/sync-scala.sh" $QUICK_FLAG
fi

# ──────────────────────────────────────────────────
# Phase: Sync Lisp dialects
# ──────────────────────────────────────────────────

if $HAS_LISP; then
    LISP_CSV=$(IFS=,; echo "${LISP_DIALECTS[*]}")
    phase_banner "Synchronizing Lisp (${LISP_CSV})"
    "$HYDRA_EXT_DIR/bin/sync-lisp.sh" --dialects "$LISP_CSV" $QUICK_FLAG
fi

banner1_done "Full sync complete!"
