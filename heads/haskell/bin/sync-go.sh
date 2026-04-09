#!/bin/bash
set -eo pipefail

# Script to synchronize Hydra-Go with the source of truth.
#
# This script regenerates all Go artifacts from the Hydra sources:
#   1. Kernel modules (type and term definitions)
#   2. Test modules
#
# Hand-written files under hydra/lib/ are preserved across regeneration.
#
# Prerequisites:
#   - Run from the hydra-ext directory
#   - Haskell must be in sync first (run sync-haskell.sh)
#
# Usage:
#   ./bin/sync-go.sh

echo "=========================================="
echo "Synchronizing Hydra-Go"
echo "=========================================="
echo ""

GEN="../hydra-go/src/gen-main/go"

# Clean generated files, preserving hand-written lib packages
echo "Cleaning generated files..."
if [ -d "$GEN/hydra" ]; then
    # Remove all generated packages except hand-written lib/
    find "$GEN/hydra" -maxdepth 1 -mindepth 1 -not -name "lib" -exec rm -rf {} +
    # Inside lib/, remove only the generated 'names' package (keep hand-written ones)
    rm -rf "$GEN/hydra/lib/names"
fi

# Generate kernel and test modules into the same tree
echo "Generating Go files..."
GENSCRIPT=$(mktemp /tmp/hydra-go-gen-XXXXXX.hs)
cat > "$GENSCRIPT" << 'HSEOF'
{-# LANGUAGE PackageImports #-}
import Hydra.Ext.Generation
import qualified "hydra-ext" Hydra.Module as M
import qualified Data.List as L

main :: IO ()
main = do
  let universe = mainModules ++ hydraExtModules
      outDir = "../hydra-go/src/gen-main/go"

  -- Step 1: Generate kernel modules
  n1 <- writeGo outDir universe kernelModules
  putStrLn $ "  Generated " ++ show n1 ++ " kernel Go files"

  -- Step 2: Find ext modules needed by test modules and generate them
  let testExtDeps = filter (\ns -> L.isPrefixOf "hydra.ext." (M.unNamespace ns))
        $ concatMap M.moduleTermDependencies testModules
      extModsForTests = filter (\m -> M.moduleNamespace m `elem` testExtDeps) universe
  if null extModsForTests
    then return ()
    else do
      n2 <- writeGo outDir universe extModsForTests
      putStrLn $ "  Generated " ++ show n2 ++ " ext Go files needed by tests"

  -- Step 3: Generate test modules
  n3 <- writeGo outDir universe testModules
  putStrLn $ "  Generated " ++ show n3 ++ " test Go files"
HSEOF
stack runghc --rts-options="-K256M -A32M" -- "$GENSCRIPT"
rm -f "$GENSCRIPT"

# Ensure go.mod exists
if [ ! -f "$GEN/go.mod" ]; then
    echo "Creating go.mod..."
    cat > "$GEN/go.mod" << 'EOF'
module hydra.dev

go 1.22.0
EOF
fi

echo ""
echo "Done. Run 'cd $GEN && go build ./...' to verify."
