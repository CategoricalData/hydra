#!/bin/bash
set -eo pipefail

# Script to regenerate hydra-ext's Haskell gen-main, JSON exports, and ext Java from Hydra sources.
#
# This regenerates:
#   1. Haskell gen-main for hydraExtModules (Java/Python coders, language syntaxes, etc.)
#   2. JSON exports for hydraExtModules
#   3. Java gen-main for ext modules needed by hydra-ext (PG, GraphSON, domain models, etc.)
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
            echo "Regenerate hydra-ext Haskell gen-main, JSON exports, and ext Java."
            echo ""
            echo "Steps performed:"
            echo "  1. Build executables"
            echo "  2. Generate ext encoder/decoder source modules"
            echo "  3. Generate Haskell ext modules"
            echo "  4. Rebuild (to pick up new Haskell files)"
            echo "  5. Export ext modules to JSON"
            exit 0
            ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

RTS_FLAGS="+RTS -K256M -A32M -RTS"

echo "=========================================="
echo "Synchronizing Hydra-Ext"
echo "=========================================="
echo ""

echo "Step 1/5: Building executables..."
echo ""
stack build \
    hydra-ext:exe:update-ext-sources \
    hydra-ext:exe:update-haskell-ext-main \
    hydra-ext:exe:update-json-ext \
    hydra-ext:exe:bootstrap-from-json

echo ""
echo "Step 2/5: Generating ext encoder/decoder source modules..."
echo ""
stack exec update-ext-sources -- $RTS_FLAGS

# Rebuild to pick up new encoder/decoder source modules
echo ""
echo "Rebuilding..."
stack build

echo ""
echo "Step 3/5: Generating Haskell ext modules..."
echo ""
stack exec update-haskell-ext-main -- $RTS_FLAGS

echo ""
echo "Step 4/5: Rebuilding..."
echo ""
stack build

echo ""
echo "Step 5/5: Exporting ext modules to JSON..."
echo ""
stack exec update-json-ext -- $RTS_FLAGS

echo ""
echo "=========================================="
echo "Sync complete!"
echo "=========================================="
