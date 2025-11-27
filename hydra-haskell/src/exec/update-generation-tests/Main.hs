#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra
-}

module Main where

import Hydra.Staging.Testing.Generation.Generate
import Hydra.Staging.Testing.Generation.HaskellCodec (haskellTestGenerator)
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Test.TestSuite as GenTests


main :: IO ()
main = do
  putStrLn "=== Generate Hydra generation tests ==="
  putStrLn ""

  -- Automatically build the lookup function from module and test group hierarchies
  let lookupFn = createTestGroupLookup TestSuite.module_ GenTests.allTests

  -- Generate generation tests to src/gen-test/haskell
  let outputDir = "src/gen-test/haskell"

  putStrLn $ "Generating tests into: " ++ outputDir
  putStrLn ""

  generateGenerationTestSuite haskellTestGenerator outputDir TestSuite.module_ lookupFn

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated tests:"
  putStrLn $ "  ls -R " ++ outputDir
  putStrLn ""
  putStrLn "To run the generated tests:"
  putStrLn "  stack test"
