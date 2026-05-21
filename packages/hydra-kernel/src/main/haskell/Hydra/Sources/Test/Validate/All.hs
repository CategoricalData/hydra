
module Hydra.Sources.Test.Validate.All where

import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep)
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

import qualified Hydra.Sources.Test.Validate.Core as ValidateCore
import qualified Hydra.Sources.Test.Validate.Packaging as ValidatePackaging


ns :: Namespace
ns = Namespace "hydra.test.validate.all"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (namespaces Prelude.++ kernelTypesNamespaces),
            moduleDescription = Just "Hydra's validation test suite"}
  where
    definitions = [Phantoms.toDefinition allTests]
    namespaces = [ValidateCore.ns, ValidatePackaging.ns]

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all validation tests" $
    Testing.testGroup (string "validation") nothing (list subgroups) (list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = [ValidateCore.allTests, ValidatePackaging.allTests]
