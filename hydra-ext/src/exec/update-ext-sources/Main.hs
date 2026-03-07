module Main where

import Hydra.Ext.Generation
import qualified Hydra.Ext.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Ext.Sources.Pg.Model as PgModel

main :: IO ()
main = do
  putStrLn "=== Generate ext encoder/decoder source modules (Haskell) ==="
  putStrLn ""

  let universe = mainModules ++ hydraExtModules
      typeModules = [PgMapping.module_, PgModel.module_]

  putStrLn "Generating decoder source modules to src/gen-main/haskell..."
  writeDecoderSourceHaskell "src/gen-main/haskell" universe typeModules

  putStrLn ""
  putStrLn "Generating encoder source modules to src/gen-main/haskell..."
  writeEncoderSourceHaskell "src/gen-main/haskell" universe typeModules

  putStrLn ""
  putStrLn "=== Done! ==="
