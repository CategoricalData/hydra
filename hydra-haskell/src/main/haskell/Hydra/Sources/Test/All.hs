-- | Aggregates all Hydra test source modules

module Hydra.Sources.Test.All where

import Hydra.Kernel

import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes


-- | All test modules (including test suite modules that TestSuite depends on)
testModules :: [Module]
testModules = baseTestModules ++ TestSuite.testSuiteModules

-- | Base test modules (TestGraph, TestTerms, TestTypes, TestSuite)
baseTestModules :: [Module]
baseTestModules = [
  TestGraph.module_,
  TestTerms.module_,
  TestTypes.module_,
  TestSuite.module_]
