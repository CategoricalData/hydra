#!/bin/bash
set -eo pipefail

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
#   Phases 3-4: Generate target languages from JSON (hydra-ext)
#     - Java, Python (from JSON via bootstrap-from-json)
#     - Haskell is already fully synced by Phases 1-2
#
# Stops at the first error. Times the entire operation.
#
# Prerequisites:
#   - Stack is installed and configured
#   - Run from the repo root (or the script will cd there)
#
# Usage:
#   ./bin/sync-all.sh                              # Full sync (default targets: hydra,java,python)
#   ./bin/sync-all.sh --quick                      # Skip tests in each phase
#   ./bin/sync-all.sh --targets all                          # All 7 implementations
#   ./bin/sync-all.sh --targets hydra,java,python,lisp       # Include Lisp dialects
#   ./bin/sync-all.sh --targets clojure,scheme               # Only specific Lisp dialects (skips hydra/java/python)
#   ./bin/sync-all.sh --help                       # Show this help
#
# Valid targets:
#   hydra    - Haskell kernel regeneration (Phase 1)
#   java     - Java code generation (Phase 3)
#   python   - Python code generation (Phase 4)
#   lisp        - All four Lisp dialects (expands to clojure,common-lisp,emacs-lisp,scheme)
#   clojure     - Clojure code generation
#   common-lisp - Common Lisp code generation
#   emacs-lisp  - Emacs Lisp code generation
#   scheme      - Scheme code generation
#
# Note: Phase 2 (hydra-ext sync) runs whenever hydra, java, python, or any Lisp target is selected.

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
            echo "  clojure     Clojure code generation"
            echo "  common-lisp Common Lisp code generation"
            echo "  emacs-lisp  Emacs Lisp code generation"
            echo "  scheme      Scheme code generation"
            echo "  lisp        All four Lisp dialects"
            echo "  all         All 7 implementations (hydra,java,python + all Lisp)"
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
            echo "Error: Unknown target '$t'"
            echo "Valid targets: hydra, java, python, scala, lisp, clojure, common-lisp, emacs-lisp, scheme, all"
            exit 1
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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"

# Ensure JAVA_HOME is set to a JDK 11+ (required for Gradle builds)
if $TARGET_JAVA; then
    if [ -z "$JAVA_HOME" ]; then
        if command -v /usr/libexec/java_home &>/dev/null; then
            export JAVA_HOME="$(/usr/libexec/java_home 2>/dev/null || true)"
        fi
    fi
    if [ -z "$JAVA_HOME" ]; then
        echo "Warning: JAVA_HOME is not set. Java compilation steps may fail."
        echo "Set JAVA_HOME to a JDK 11+ installation before running this script."
    else
        echo "Using JAVA_HOME=$JAVA_HOME"
    fi

    # Warn if running an x86_64 JDK under Rosetta on Apple Silicon (causes ~20x slowdown)
    if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
        JAVA_CMD="${JAVA_HOME:+$JAVA_HOME/bin/}java"
        if command -v "$JAVA_CMD" > /dev/null 2>&1 && file "$(command -v "$JAVA_CMD")" | grep -q x86_64; then
            echo "WARNING: x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2"
            echo "  and will be ~20x slower than a native arm64 JDK."
            echo "  Current JDK: $("$JAVA_CMD" -version 2>&1 | head -1)"
            echo ""
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

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

# Count total phases for display
TOTAL_PHASES=0
if $TARGET_HYDRA; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $NEED_EXT; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $TARGET_JAVA; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $TARGET_PYTHON; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $TARGET_SCALA; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
if $HAS_LISP; then TOTAL_PHASES=$((TOTAL_PHASES + 1)); fi
CURRENT_PHASE=0

echo "============================================"
echo "Hydra full sync (targets: $TARGETS)"
echo "============================================"
echo ""

QUICK_FLAG=""
if [ "$QUICK_MODE" = true ]; then
    QUICK_FLAG="--quick"
fi

# ──────────────────────────────────────────────────
# Phase: Generate Haskell from DSL (hydra-haskell)
# ──────────────────────────────────────────────────

if $TARGET_HYDRA; then
    CURRENT_PHASE=$((CURRENT_PHASE + 1))

    echo "============================================"
    echo "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: Generating Haskell from DSL"
    echo "============================================"
    echo ""

    cd "$HYDRA_HASKELL_DIR"

    echo "Step 1a: Generating kernel modules..."
    echo ""
    stack build hydra:exe:update-haskell-kernel
    stack exec update-haskell-kernel -- $RTS_FLAGS
    echo ""
    echo "Rebuilding..."
    stack build

    echo ""
    echo "Step 1b: Generating kernel test modules..."
    echo ""
    stack build hydra:exe:update-kernel-tests
    stack exec update-kernel-tests -- $RTS_FLAGS
    echo ""
    echo "Rebuilding..."
    stack build

    echo ""
    echo "Step 1c: Generating eval lib modules..."
    echo ""
    stack build hydra:exe:update-haskell-eval-lib
    stack exec update-haskell-eval-lib -- $RTS_FLAGS
    echo ""
    echo "Rebuilding..."
    stack build

    echo ""
    echo "Step 1d: Generating encoder/decoder source modules..."
    echo ""
    stack build hydra:exe:update-haskell-sources
    stack exec update-haskell-sources -- $RTS_FLAGS
    echo ""
    echo "Rebuilding..."
    stack build

    echo ""
    echo "Step 1e: Regenerating kernel modules (post encoder/decoder)..."
    echo ""
    stack exec update-haskell-kernel -- $RTS_FLAGS
    echo ""
    echo "Rebuilding..."
    stack build

    echo ""
    echo "Step 1f: Generating generation tests..."
    echo ""
    stack build hydra:exe:update-generation-tests
    stack exec update-generation-tests -- $RTS_FLAGS
    echo ""
    echo "Rebuilding..."
    stack build

    if [ "$QUICK_MODE" = false ]; then
        echo ""
        echo "Step 1g: Running Haskell tests..."
        echo ""
        stack test 2>&1
    fi

    echo ""
    echo "Step 1h: Exporting and verifying JSON..."
    echo ""
    stack build hydra:exe:update-json-main hydra:exe:verify-json-kernel hydra:exe:update-json-test
    stack exec update-json-main -- $RTS_FLAGS
    stack exec update-json-test -- $RTS_FLAGS
    stack exec verify-json-kernel -- $RTS_FLAGS

    echo ""
    echo "Step 1i: Generating JSON manifest..."
    echo ""
    stack build hydra:exe:update-json-manifest
    stack exec update-json-manifest

    echo ""
    echo "Phase ${CURRENT_PHASE} complete."
    echo ""
fi

# ──────────────────────────────────────────────────
# Phase: Generate ext modules (hydra-ext)
# ──────────────────────────────────────────────────

if $NEED_EXT; then
    CURRENT_PHASE=$((CURRENT_PHASE + 1))

    echo "============================================"
    echo "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: Synchronizing Hydra-Ext"
    echo "============================================"
    echo ""

    "$HYDRA_EXT_DIR/bin/sync-ext.sh"

    echo ""
fi

# ──────────────────────────────────────────────────
# Phase: Sync Java from JSON
# ──────────────────────────────────────────────────

if $TARGET_JAVA; then
    CURRENT_PHASE=$((CURRENT_PHASE + 1))

    echo "============================================"
    echo "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: Synchronizing Java (from JSON)"
    echo "============================================"
    echo ""

    "$HYDRA_EXT_DIR/bin/sync-java.sh" $QUICK_FLAG

    echo ""
fi

# ──────────────────────────────────────────────────
# Phase: Sync Python from JSON
# ──────────────────────────────────────────────────

if $TARGET_PYTHON; then
    CURRENT_PHASE=$((CURRENT_PHASE + 1))

    echo "============================================"
    echo "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: Synchronizing Python (from JSON)"
    echo "============================================"
    echo ""

    "$HYDRA_EXT_DIR/bin/sync-python.sh" $QUICK_FLAG

    echo ""
fi

# ──────────────────────────────────────────────────
# Phase: Sync Scala
# ──────────────────────────────────────────────────

if $TARGET_SCALA; then
    CURRENT_PHASE=$((CURRENT_PHASE + 1))

    echo "============================================"
    echo "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: Synchronizing Scala"
    echo "============================================"
    echo ""

    "$HYDRA_EXT_DIR/bin/sync-scala.sh" $QUICK_FLAG

    echo ""
fi

# ──────────────────────────────────────────────────
# Phase: Sync Lisp dialects
# ──────────────────────────────────────────────────

if $HAS_LISP; then
    CURRENT_PHASE=$((CURRENT_PHASE + 1))

    LISP_CSV=$(IFS=,; echo "${LISP_DIALECTS[*]}")

    echo "============================================"
    echo "Phase ${CURRENT_PHASE}/${TOTAL_PHASES}: Synchronizing Lisp (${LISP_CSV})"
    echo "============================================"
    echo ""

    "$HYDRA_EXT_DIR/bin/sync-lisp.sh" --dialects "$LISP_CSV" $QUICK_FLAG

    echo ""
fi

echo "============================================"
echo "Full sync complete!"
echo "============================================"
