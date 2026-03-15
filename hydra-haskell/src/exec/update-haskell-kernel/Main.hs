module Main where

import Hydra.Generation
import Hydra.Sources.All

main :: IO ()
main = do
  putStrLn "=== Generate Hydra kernel (Haskell) ==="
  putStrLn ""
  putStrLn "Generating main modules to src/gen-main/haskell..."
  putStrLn ""

  writeHaskell "src/gen-main/haskell" mainModules mainModules

  putStrLn ""
  putStrLn "Generating DSL source module (hydra.dsls) to src/gen-main/haskell..."
  putStrLn ""

  writeDslSourceHaskell "src/gen-main/haskell"

  putStrLn ""
  putStrLn "=== Done! ==="
