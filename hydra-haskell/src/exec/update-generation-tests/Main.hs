#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra
-}

module Main where

import Hydra.Kernel
import Hydra.Staging.Testing.Generation.Generate
import Hydra.Staging.Testing.Generation.HaskellCodec (haskellTestGenerator)
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Test.TestSuite as GenTests


main :: IO ()
main = do
  putStrLn "=== Generate Hydra generation tests ==="
  putStrLn ""

  -- Get the namespaces from TestSuite's term dependencies
  let testNamespaces = moduleTermDependencies TestSuite.module_

  -- Build the lookup function from namespaces and test group hierarchy
  let lookupFn = createTestGroupLookup testNamespaces GenTests.allTests

  -- Get the list of test modules by looking up each namespace
  -- For now, use the libPairs and otherPairs from TestSuite (the actual Module values)
  let testModules = TestSuite.testSuiteModules

  -- Generate generation tests to src/gen-test/haskell
  let outputDir = "src/gen-test/haskell"

  putStrLn $ "Generating tests into: " ++ outputDir
  putStrLn ""

  generateGenerationTestSuite haskellTestGenerator outputDir testModules lookupFn

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated tests:"
  putStrLn $ "  ls -R " ++ outputDir
  putStrLn ""
  putStrLn "To run the generated tests:"
  putStrLn "  stack test"
