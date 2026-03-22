
module Hydra.Sources.Test.Hoisting.All where

import Hydra.Kernel
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

import qualified Hydra.Sources.Test.Hoisting.Cases as Cases
import qualified Hydra.Sources.Test.Hoisting.Let as Let


ns :: Namespace
ns = Namespace "hydra.test.hoisting.all"

module_ :: Module
module_ = Module ns elements namespaces kernelTypesNamespaces $
    Just "Hydra's hoisting test suite"
  where
    elements = [Phantoms.toTermDefinition allTests]
    namespaces = [
      Cases.ns,
      Let.ns]

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all hoisting tests" $
    Testing.testGroup (string "hoisting") nothing (list subgroups) (list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = [
      Cases.allTests,
      Let.allTests]
