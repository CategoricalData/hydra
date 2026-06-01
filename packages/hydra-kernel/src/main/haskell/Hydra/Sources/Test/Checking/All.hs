
module Hydra.Sources.Test.Checking.All where

import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Sources.Kernel.Types.All
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M

import qualified Hydra.Sources.Test.Checking.Advanced as Advanced
import qualified Hydra.Sources.Test.Checking.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Sources.Test.Checking.Collections as Collections
import qualified Hydra.Sources.Test.Checking.Failures as Failures
import qualified Hydra.Sources.Test.Checking.Fundamentals as Fundamentals
import qualified Hydra.Sources.Test.Checking.NominalTypes as NominalTypes


ns :: ModuleName
ns = ModuleName "hydra.test.checking.all"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (namespaces Prelude.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Hydra's type checking test suite")}
  where
    definitions = [Phantoms.toDefinition allTests]
    namespaces = [
      Advanced.ns,
      AlgebraicTypes.ns,
      Collections.ns,
      Failures.ns,
      Fundamentals.ns,
      NominalTypes.ns,
      ModuleName "hydra.rewriting"]

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all type checking tests" $
    Testing.testGroup (string "checking") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [
      Advanced.allTests,
      AlgebraicTypes.allTests,
      Collections.allTests,
      Failures.allTests,
      Fundamentals.allTests,
      NominalTypes.allTests]
