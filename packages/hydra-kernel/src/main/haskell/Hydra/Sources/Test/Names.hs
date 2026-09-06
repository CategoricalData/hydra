module Hydra.Sources.Test.Names where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Sources.Kernel.Terms.Names as Names

import Hydra.Testing


ns :: ModuleName
ns = ModuleName "hydra.test.names"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, Names.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for qualified-name construction"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition composeProvisionNameTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for hydra.names" $
    Testing.testGroup (string "names") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [
      composeProvisionNameTests]

composeProvisionNameTests :: TypedTermDefinition TestGroup
composeProvisionNameTests = define "composeProvisionNameTests" $
  doc "Test cases for composeProvisionName" $
  Testing.testGroup (string "composeProvisionName") nothing (list ([] :: [TypedTerm TestGroup])) (list match)
  where
    match = [
      testCase "definition with namespace"
        (Core.name $ string "hydra.lib.lists.concat") (string "emptyLists")
        (string "hydra.lib.lists.concat.emptyLists"),
      testCase "local name with no namespace"
        (Core.name $ string "concat") (string "emptyLists")
        (string "concat.emptyLists"),
      testCase "module name as the enclosing entity"
        (Core.name $ string "hydra.core") (string "termOrdering")
        (string "hydra.core.termOrdering"),
      testCase "composing onto an already-composed provision name (module > definition > provision)"
        (Names.composeProvisionName @@ Core.name (string "hydra.lib.lists") @@ string "concat")
        (string "emptyLists")
        (string "hydra.lib.lists.concat.emptyLists")]

-- Helpers

testCase :: String -> TypedTerm Name -> TypedTerm String -> TypedTerm String -> TypedTerm TestCaseWithMetadata
testCase name entityName localName expected =
  universalCase name actual expected
  where
    actual = Core.unName (Names.composeProvisionName @@ entityName @@ localName)
