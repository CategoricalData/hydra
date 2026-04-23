-- | Aggregates all Hydra test source modules

module Hydra.Sources.Test.All where

import Hydra.Kernel

import qualified Hydra.Sources.Test.TestEnv as TestEnv
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes


-- | All test modules (including test suite modules that TestSuite depends on)
testModules :: [Module]
testModules = baseTestModules ++ TestSuite.testSuiteModules

-- | Base test modules (TestEnv is a type-only stub; TestGraph references it).
-- TestEnv has hand-written per-language runtime counterparts and must not be
-- emitted as generated source — bootstrap-from-json skips it by namespace.
baseTestModules :: [Module]
baseTestModules = [
  TestEnv.module_,
  TestGraph.module_,
  TestTerms.module_,
  TestTypes.module_,
  TestSuite.module_]

-- | Namespaces that are in the test universe for type inference but whose
-- source should NOT be emitted by bootstrap-from-json. The hand-written
-- per-language counterparts are the source of truth.
testSkipEmitNamespaces :: [Namespace]
testSkipEmitNamespaces = [TestEnv.ns]
