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
#   ./bin/sync-all.sh          # Full sync (all steps including tests)
#   ./bin/sync-all.sh --quick  # Skip tests in each phase
#   ./bin/sync-all.sh --help   # Show this help

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
            echo "Run all Hydra sync steps in the correct order."
            echo ""
            echo "Options:"
            echo "  --quick    Skip tests in each phase"
            echo "  --help     Show this help message"
            echo ""
            echo "Phases:"
            echo "  1. Generate Haskell from DSL (kernel, tests, eval lib, sources, JSON)"
            echo "  2. Generate ext modules and JSON (hydra-ext)"
            echo "  3. Sync Java from JSON"
            echo "  4. Sync Python from JSON"
            echo ""
            echo "Stops at the first error. Reports total elapsed time."
            exit 0
            ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"

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

echo "============================================"
echo "Hydra full sync"
echo "============================================"
echo ""

# ──────────────────────────────────────────────────
# Phase 1: Generate Haskell from DSL (hydra-haskell)
# ──────────────────────────────────────────────────

echo "============================================"
echo "Phase 1/4: Generating Haskell from DSL"
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
stack build hydra:exe:update-json-kernel hydra:exe:update-json-main hydra:exe:verify-json-kernel
stack exec update-json-kernel -- $RTS_FLAGS
stack exec update-json-main -- $RTS_FLAGS
stack exec verify-json-kernel -- $RTS_FLAGS

echo ""
echo "Step 1i: Generating JSON manifest..."
echo ""
stack build hydra:exe:update-json-manifest
stack exec update-json-manifest

echo ""
echo "Phase 1 complete."
echo ""

# ──────────────────────────────────────────────────
# Phase 2: Generate ext modules (hydra-ext)
# ──────────────────────────────────────────────────

echo "============================================"
echo "Phase 2/4: Synchronizing Hydra-Ext"
echo "============================================"
echo ""

"$HYDRA_EXT_DIR/bin/sync-ext.sh"

echo ""

# ──────────────────────────────────────────────────
# Phases 3-4: Sync Java and Python from JSON
# ──────────────────────────────────────────────────
# Note: Haskell is already fully synced by Phases 1-2 (DSL generation + JSON export).
# hydra-ext/bin/sync-haskell.sh exists for standalone use but is not needed here.

QUICK_FLAG=""
if [ "$QUICK_MODE" = true ]; then
    QUICK_FLAG="--quick"
fi

echo "============================================"
echo "Phase 3/4: Synchronizing Java (from JSON)"
echo "============================================"
echo ""

"$HYDRA_EXT_DIR/bin/sync-java.sh" $QUICK_FLAG

echo ""
echo "============================================"
echo "Phase 4/4: Synchronizing Python (from JSON)"
echo "============================================"
echo ""

"$HYDRA_EXT_DIR/bin/sync-python.sh" $QUICK_FLAG

echo ""
echo "============================================"
echo "Full sync complete!"
echo "============================================"
