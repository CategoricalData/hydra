
-- | Type checking failure test cases
module Hydra.Sources.Test.Checking.Failures where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M


ns :: Namespace
ns = Namespace "hydra.test.checking.failures"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns, Namespace "hydra.rewriting"]
    kernelTypesNamespaces
    (Just "Type checking failure test cases")
  where
    elements = [
      Phantoms.toTermDefinition allTests,
      Phantoms.toTermDefinition failOnUntypedTests,
      Phantoms.toTermDefinition untypedLambdasTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Type checking failure test cases" $
  supergroup "Failures" [
    failOnUntypedTests]

------ Helper functions ------

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTests :: TBinding TestGroup
failOnUntypedTests = define "failOnUntypedTests" $
  supergroup "Fail on untyped (pre-inference) terms" [
    untypedLambdasTests]

untypedLambdasTests :: TBinding TestGroup
untypedLambdasTests = define "untypedLambdasTests" $
  subgroup "Untyped lambdas" ([] :: [TTerm TestCaseWithMetadata])
