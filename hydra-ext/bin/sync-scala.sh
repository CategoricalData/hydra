#!/bin/bash
set -eo pipefail

# Script to synchronize Hydra-Scala with the source of truth in Hydra-Haskell/Hydra-Ext.
#
# This script regenerates all Scala artifacts from the Hydra sources:
#   1. Build hydra-ext (compiles the Scala coder DSL)
#   2. Regenerate Haskell ext-main (generates Haskell from DSL)
#   3. Rebuild hydra-ext (compiles the generated Haskell)
#   4. Generate Scala source modules
#   5. Post-process generated Scala (break long lines, fix escapes)
#   6. Compile Scala (unless --quick)
#
# Prerequisites:
#   - Hydra-Haskell must be built and up to date (run sync-haskell.sh first)
#   - Run from the hydra-ext directory
#
# Usage:
#   ./bin/sync-scala.sh          # Full sync (all steps)
#   ./bin/sync-scala.sh --quick  # Skip Scala compilation

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
            echo "Synchronize Hydra-Scala with the source of truth in Hydra-Haskell/Hydra-Ext."
            echo ""
            echo "Options:"
            echo "  --quick    Skip Scala compilation after generation"
            echo "  --help     Show this help message"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Hydra-Scala"
echo "=========================================="
echo ""

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/.." && pwd )"
HYDRA_SCALA_DIR="$HYDRA_ROOT_DIR/hydra-scala"

cd "$HYDRA_EXT_DIR"

RTS_FLAGS="+RTS -K256M -A32M -RTS"
TOTAL_STEPS=3
if [ "$QUICK_MODE" = true ]; then
    TOTAL_STEPS=2
fi

echo "Step 1/$TOTAL_STEPS: Building hydra-ext..."
echo ""
stack build

echo ""
echo "Step 2/$TOTAL_STEPS: Generating Scala source modules..."
echo ""
stack exec update-scala -- $RTS_FLAGS
python3 "$HYDRA_SCALA_DIR/bin/break-long-lines.py"

# Fix Scala 3 reserved word 'macro' in generated enum cases and pattern matches
echo "  Post-processing: escaping 'macro' keyword..."
find "$HYDRA_SCALA_DIR/src/gen-main/scala" -name "*.scala" -exec \
    sed -i '' -e 's/case macro(/case `macro`(/g' -e 's/\.macro(/.`macro`(/g' {} +

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 3/$TOTAL_STEPS: Compiling Scala..."
    echo ""
    cd "$HYDRA_SCALA_DIR"
    sbt compile
    cd "$HYDRA_EXT_DIR"
else
    echo ""
    echo "Step 3/$TOTAL_STEPS: Skipped (--quick mode)"
fi

echo ""
echo "=========================================="
echo "Hydra-Scala sync complete!"
echo "=========================================="
