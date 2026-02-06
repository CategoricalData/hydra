module Main where

import Hydra.Generation
import Hydra.Sources.All
import Hydra.Sources.Eval.Lib.All

main :: IO ()
main = do
  putStrLn "=== Generate Hydra eval lib (Haskell) ==="
  putStrLn ""
  putStrLn "Generating eval lib modules to src/gen-main/haskell..."
  putStrLn ""

  writeHaskell "src/gen-main/haskell" mainModules evalLibModules

  putStrLn ""
  putStrLn "=== Done! ==="
