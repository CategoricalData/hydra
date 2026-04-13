module Main where

import Hydra.ExtGeneration
import qualified Hydra.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Sources.Pg.Model as PgModel

main :: IO ()
main = do
  putStrLn "=== Generate ext encoder/decoder source modules (Haskell) ==="
  putStrLn ""

  let universe = mainModules ++ hydraExtModules
      typeModules = [PgMapping.module_, PgModel.module_]

  putStrLn "Generating decoder source modules to dist/haskell/hydra-pg/src/main/haskell..."
  writeDecoderSourceHaskell "../../dist/haskell/hydra-pg/src/main/haskell" universe typeModules

  putStrLn ""
  putStrLn "Generating encoder source modules to dist/haskell/hydra-pg/src/main/haskell..."
  writeEncoderSourceHaskell "../../dist/haskell/hydra-pg/src/main/haskell" universe typeModules

  putStrLn ""
  putStrLn "=== Done! ==="
