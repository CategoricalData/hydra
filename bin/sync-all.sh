#!/bin/bash
set -e

# Top-level synchronization script for Hydra.
#
# Runs all sync scripts in the correct order:
#   1. sync-haskell.sh  (hydra-haskell) -- regenerate Haskell kernel, tests, JSON
#   2. sync-ext.sh      (hydra-ext)     -- regenerate ext Haskell modules and JSON exports
#   3. sync-java.sh     (hydra-ext)     -- regenerate Java kernel, tests, eval lib
#   4. sync-python.sh   (hydra-ext)     -- regenerate Python kernel, tests, eval lib
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

QUICK_FLAG=""

for arg in "$@"; do
    case $arg in
        --quick)
            QUICK_FLAG="--quick"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Run all Hydra sync scripts in the correct order."
            echo ""
            echo "Options:"
            echo "  --quick    Pass --quick to each sync script (skip tests)"
            echo "  --help     Show this help message"
            echo ""
            echo "Scripts run in order:"
            echo "  1. hydra-haskell/bin/sync-haskell.sh"
            echo "  2. hydra-ext/bin/sync-ext.sh"
            echo "  3. hydra-ext/bin/sync-java.sh"
            echo "  4. hydra-ext/bin/sync-python.sh"
            echo ""
            echo "Stops at the first error. Reports total elapsed time."
            exit 0
            ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

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

echo "============================================"
echo "Hydra full sync"
echo "============================================"
echo ""

echo "Phase 1/4: Synchronizing Haskell..."
echo ""
"$HYDRA_ROOT/hydra-haskell/bin/sync-haskell.sh" $QUICK_FLAG

echo ""
echo "Phase 2/4: Synchronizing Hydra-Ext..."
echo ""
"$HYDRA_ROOT/hydra-ext/bin/sync-ext.sh"

echo ""
echo "Phase 3/4: Synchronizing Java..."
echo ""
"$HYDRA_ROOT/hydra-ext/bin/sync-java.sh" $QUICK_FLAG

echo ""
echo "Phase 4/4: Synchronizing Python..."
echo ""
"$HYDRA_ROOT/hydra-ext/bin/sync-python.sh" $QUICK_FLAG

echo ""
echo "============================================"
echo "Full sync complete!"
echo "============================================"
