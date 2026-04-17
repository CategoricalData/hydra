#!/usr/bin/env bash
set -euo pipefail

# Script to regenerate hydra-ext's Haskell modules, JSON exports, and ext Java from Hydra sources.
#
# This regenerates:
#   1. Haskell modules for hydraExtModules (Java/Python coders, language syntaxes, etc.)
#   2. JSON exports for hydraExtModules
#   3. Java modules for ext modules needed by hydra-ext (PG, GraphSON, domain models, etc.)
#   4. WebAssembly text format (WAT) files for the kernel modules
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
            echo "Regenerate hydra-ext Haskell modules, JSON exports, and ext Java."
            echo ""
            echo "Steps performed:"
            echo "  1. Build executables"
            echo "  2. Generate ext encoder/decoder source modules"
            echo "  3. Generate Haskell ext modules"
            echo "  4. Rebuild (to pick up new Haskell files)"
            echo "  5. Export ext modules to JSON"
            echo "  6. Generate WebAssembly text format (WAT) files"
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

TOTAL_STEPS=6

step 1 $TOTAL_STEPS "Building executables"
echo ""
stack build \
    hydra:exe:update-ext-sources \
    hydra:exe:update-haskell-ext-main \
    hydra:exe:update-json-ext \
    hydra:exe:update-wasm \
    hydra:exe:bootstrap-from-json

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

step 6 $TOTAL_STEPS "Generating WebAssembly text format (WAT) files"
echo ""
stack exec update-wasm -- $RTS_FLAGS

banner2_done "Hydra-Ext sync complete!"
