#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra
-}

module Main where

import Hydra.Generation
import Hydra.Sources.All

main :: IO ()
main = do
  putStrLn "=== Generate Hydra kernel tests ==="
  putStrLn ""
  putStrLn "Generating kernel test modules to src/gen-test/haskell..."
  putStrLn ""

  writeHaskell "src/gen-test/haskell" testModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated test modules:"
  putStrLn "  ls -R src/gen-test/haskell"
  putStrLn ""
  putStrLn "To run the tests:"
  putStrLn "  stack test"
