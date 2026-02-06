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
  putStrLn "=== Done! ==="
