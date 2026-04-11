#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra
-}

module Main where

import Hydra.Haskell.Generation (writeHaskell)
import Hydra.Sources.All

main :: IO ()
main = do
  let outDir = "../hydra-kernel/src/gen-test/haskell"
  putStrLn "=== Generate Hydra kernel tests ==="
  putStrLn ""
  putStrLn $ "Generating kernel test modules to " ++ outDir ++ "..."
  putStrLn ""

  -- Universe provides all modules for dependency resolution
  -- testModules now includes all test suite modules (including Reduction, etc.)
  -- writeHaskell computes transitive closure to include only what's needed
  let allModules = mainModules ++ testModules
  writeHaskell outDir allModules testModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated test modules:"
  putStrLn $ "  ls -R " ++ outDir
  putStrLn ""
  putStrLn "To run the tests:"
  putStrLn "  stack test"
