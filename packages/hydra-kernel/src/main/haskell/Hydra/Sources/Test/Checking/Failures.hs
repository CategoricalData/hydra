
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
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [TestGraph.ns, Namespace "hydra.rewriting"],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = (Just "Type checking failure test cases")}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition failOnUntypedTests,
      Phantoms.toDefinition untypedLambdasTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Type checking failure test cases" $
  supergroup "Failures" [
    failOnUntypedTests]

------ Helper functions ------

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTests :: TTermDefinition TestGroup
failOnUntypedTests = define "failOnUntypedTests" $
  supergroup "Fail on untyped (pre-inference) terms" [
    untypedLambdasTests]

untypedLambdasTests :: TTermDefinition TestGroup
untypedLambdasTests = define "untypedLambdasTests" $
  subgroup "Untyped lambdas" ([] :: [TTerm TestCaseWithMetadata])
