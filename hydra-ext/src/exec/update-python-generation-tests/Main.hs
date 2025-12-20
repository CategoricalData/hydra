#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra-ext
-}

module Main where

import Hydra.Kernel
import Hydra.Staging.Testing.Generation.Generate
import Hydra.Ext.Staging.Python.TestCodec (pythonTestGenerator)
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Test.TestSuite as GenTests
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "=== Generate Python generation tests ==="
  putStrLn ""

  -- Get the namespaces from TestSuite's term dependencies
  let testNamespaces = moduleTermDependencies TestSuite.module_

  -- Build the lookup function from namespaces and test group hierarchy
  let lookupFn = createTestGroupLookup testNamespaces GenTests.allTests

  -- Get the list of test modules explicitly
  let testModules = TestSuite.testSuiteModules

  -- Generate generation tests to ../hydra-python/src/gen-test/python
  let outputDir = "../hydra-python/src/gen-test/python"

  putStrLn $ "Generating tests into: " ++ outputDir
  putStrLn ""

  generateGenerationTestSuite pythonTestGenerator outputDir testModules lookupFn

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated tests:"
  putStrLn $ "  ls -R " ++ outputDir
  putStrLn ""
  putStrLn "To run the generated tests:"
  putStrLn "  cd ../hydra-python && pytest src/gen-test/python -v"
