#!/usr/bin/env bash
set -euo pipefail

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

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/../.." && pwd )"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

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
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

cd "$HYDRA_EXT_DIR"

banner2 "Synchronizing Hydra-Ext"
echo ""

TOTAL_STEPS=5

step 1 $TOTAL_STEPS "Building executables"
echo ""
stack build \
    hydra-ext:exe:update-ext-sources \
    hydra-ext:exe:update-haskell-ext-main \
    hydra-ext:exe:update-json-ext \
    hydra-ext:exe:bootstrap-from-json

step 2 $TOTAL_STEPS "Generating ext encoder/decoder source modules"
echo ""
stack exec update-ext-sources -- $RTS_FLAGS

# Rebuild to pick up new encoder/decoder source modules
echo ""
echo "  Rebuilding..."
stack build

step 3 $TOTAL_STEPS "Generating Haskell ext modules"
echo ""
stack exec update-haskell-ext-main -- $RTS_FLAGS

# Clean up empty generated files (produced for modules with no DSL output)
find ../../dist/haskell/hydra-ext/src/main/haskell -name "*.hs" -empty -delete 2>/dev/null || true

step 4 $TOTAL_STEPS "Rebuilding"
echo ""
stack build

step 5 $TOTAL_STEPS "Exporting ext modules to JSON"
echo ""
stack exec update-json-ext -- $RTS_FLAGS

banner2_done "Hydra-Ext sync complete!"
