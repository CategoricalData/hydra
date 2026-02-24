#!/bin/bash
set -eo pipefail

# Script to regenerate hydra-ext's Haskell gen-main and JSON exports from Hydra sources.
#
# This regenerates:
#   1. Haskell gen-main for hydraExtModules (Java/Python coders, language syntaxes, etc.)
#   2. JSON exports for hydraExtModules
#
# This must be run AFTER sync-haskell.sh and BEFORE sync-java.sh or sync-python.sh,
# since those scripts depend on hydra-ext's generated Haskell code being up to date.
#
# Prerequisites:
#   - Hydra-Haskell must be consistent (run hydra-haskell/bin/sync-haskell.sh first)
#   - Run from the hydra-ext directory (or the script will cd there)
#
# Usage:
#   ./bin/sync-ext.sh          # Full sync
#   ./bin/sync-ext.sh --help   # Show this help

for arg in "$@"; do
    case $arg in
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Regenerate hydra-ext Haskell gen-main and JSON exports."
            echo ""
            echo "Steps performed:"
            echo "  1. Build executables"
            echo "  2. Generate Haskell ext modules"
            echo "  3. Rebuild (to pick up new Haskell files)"
            echo "  4. Export ext modules to JSON"
            exit 0
            ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "=========================================="
echo "Synchronizing Hydra-Ext"
echo "=========================================="
echo ""

echo "Step 1/4: Building executables..."
echo ""
stack build \
    hydra-ext:exe:update-haskell-ext-main \
    hydra-ext:exe:update-json-ext

echo ""
echo "Step 2/4: Generating Haskell ext modules..."
echo ""
stack exec update-haskell-ext-main -- $RTS_FLAGS

echo ""
echo "Step 3/4: Rebuilding..."
echo ""
stack build

echo ""
echo "Step 4/4: Exporting ext modules to JSON..."
echo ""
stack exec update-json-ext -- $RTS_FLAGS

echo ""
echo "=========================================="
echo "Sync complete!"
echo "=========================================="
