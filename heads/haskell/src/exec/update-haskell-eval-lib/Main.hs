module Main where

import Hydra.Haskell.Generation (writeHaskell)
import Hydra.Sources.All
import Hydra.Sources.Eval.Lib.All

main :: IO ()
main = do
  putStrLn "=== Generate Hydra eval lib (Haskell) ==="
  putStrLn ""
  let outDir = "../../packages/hydra-kernel/src/gen-main/haskell"
  putStrLn $ "Generating eval lib modules to " ++ outDir ++ "..."
  putStrLn ""

  writeHaskell outDir mainModules evalLibModules

  putStrLn ""
  putStrLn "=== Done! ==="
