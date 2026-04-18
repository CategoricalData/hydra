#!/usr/bin/env bash
set -euo pipefail

# Temporary per-package sync for hydra-wasm.
#
# Regenerates just the hydra-wasm pipeline:
#   1. Build the Haskell head
#   2. Export hydra-wasm DSL modules to JSON
#      (dist/json/hydra-wasm/src/main/json/)
#   3. Write per-package JSON manifests (refreshes hydra-wasm's manifest.json)
#   4. Generate WAT files to dist/wasm/
#
# This avoids the heavyweight update-haskell-ext-main step and produces
# fresh WAT output in a few minutes rather than ~17 CPU-min. The Haskell
# coder tree at dist/haskell/hydra-wasm/ is not regenerated here; for that,
# fall back to the existing sync-ext.sh (until feature_290_packaging's
# commit C3 lands and bootstrap-from-json --package hydra-wasm takes over).
#
# Delete this script once bin/sync.sh handles hydra-wasm directly (pending
# commits C5/C6 of feature_290_packaging's task #3).
#
# Usage:
#   ./bin/sync-wasm.sh          # Full sync
#   ./bin/sync-wasm.sh --help   # Show this help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

source "$HYDRA_ROOT_DIR/bin/lib/common.sh"

for arg in "$@"; do
    case $arg in
        --help|-h)
            sed -n '3,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            die "Unknown argument: $arg (try --help)"
            ;;
    esac
done

cd "$HYDRA_HASKELL_DIR"

banner2 "Synchronizing Hydra-Wasm"
echo ""

TOTAL_STEPS=4

step 1 $TOTAL_STEPS "Building executables"
echo ""
stack build \
    hydra:exe:transform-haskell-dsl-to-json \
    hydra:exe:update-json-manifest \
    hydra:exe:update-wasm

step 2 $TOTAL_STEPS "Exporting hydra-wasm DSL modules to JSON"
echo ""
stack exec transform-haskell-dsl-to-json -- --package hydra-wasm --source-set main $RTS_FLAGS

step 3 $TOTAL_STEPS "Writing per-package JSON manifests"
echo ""
stack exec update-json-manifest -- $RTS_FLAGS

step 4 $TOTAL_STEPS "Generating WebAssembly text format (WAT) files"
echo ""
stack exec update-wasm -- $RTS_FLAGS

banner2_done "Hydra-Wasm sync complete!"
