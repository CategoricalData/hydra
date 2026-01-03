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

  -- Universe provides all modules for dependency resolution
  -- testModules now includes all test suite modules (including Reduction, etc.)
  -- writeHaskell computes transitive closure to include only what's needed
  let allModules = mainModules ++ testModules
  writeHaskell "src/gen-test/haskell" allModules testModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated test modules:"
  putStrLn "  ls -R src/gen-test/haskell"
  putStrLn ""
  putStrLn "To run the tests:"
  putStrLn "  stack test"
