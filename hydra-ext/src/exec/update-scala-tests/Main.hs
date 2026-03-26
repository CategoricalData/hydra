module Main where

import Hydra.Ext.Generation
import Hydra.Generation (createTestGroupLookup)
import Hydra.Kernel (moduleNamespace, moduleTermDependencies)
import Hydra.Ext.Scala.TestingIo (scalaTestGenerator, generateScalaGenerationTests)
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Test.TestSuite as GenTests


main :: IO ()
main = do
  putStrLn "=== Generate Scala test modules and generation tests ==="
  putStrLn ""

  let allMainModules = mainModules ++ hydraBootstrapCoderModules
  let universeModules = allMainModules ++ testModules

  -- Step 1: Generate test suite modules (TestSuite, TestGraph, TestTerms, etc.)
  -- to src/gen-test/scala/ using the Scala coder
  let testOutputDir = "../hydra-scala/src/gen-test/scala"
  putStrLn "Generating test suite modules..."
  n <- writeScala testOutputDir universeModules testModules
  putStrLn $ "  Generated " ++ show n ++ " test suite files"
  putStrLn ""

  -- Step 2: Generate generation tests (delegated evaluation assert tests)
  -- to src/gen-test/scala/generation/
  let genTestOutputDir = "../hydra-scala/src/gen-test/scala/generation"
  putStrLn "Generating generation tests..."

  let testNamespaces = moduleTermDependencies TestSuite.module_
  let lookupFn = createTestGroupLookup testNamespaces GenTests.allTests
  let testSuiteModules = TestSuite.testSuiteModules

  generateScalaGenerationTests genTestOutputDir testSuiteModules lookupFn

  putStrLn ""
  putStrLn "=== Done! ==="
