
-- | Type checking failure test cases
module Hydra.Sources.Test.Checking.Failures where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M


ns :: ModuleName
ns = ModuleName "hydra.core.test.checking.failures"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.core.rewriting"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Type checking failure test cases"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition failOnUntypedTests,
      Phantoms.toDefinition untypedLambdasTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Type checking failure test cases" $
  supergroup "Failures" [
    failOnUntypedTests]

------ Helper functions ------

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTests :: TypedTermDefinition TestGroup
failOnUntypedTests = define "failOnUntypedTests" $
  supergroup "Fail on untyped (pre-inference) terms" [
    untypedLambdasTests]

untypedLambdasTests :: TypedTermDefinition TestGroup
untypedLambdasTests = define "untypedLambdasTests" $
  subgroup "Untyped lambdas" ([] :: [TypedTerm TestCaseWithMetadata])
