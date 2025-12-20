
module Hydra.Sources.Test.Inference.All where

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

import qualified Hydra.Sources.Test.Inference.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Sources.Test.Inference.AlgorithmW as AlgorithmW
import qualified Hydra.Sources.Test.Inference.Failures as Failures
import qualified Hydra.Sources.Test.Inference.Fundamentals as Fundamentals
import qualified Hydra.Sources.Test.Inference.KernelExamples as KernelExamples
import qualified Hydra.Sources.Test.Inference.NominalTypes as NominalTypes


ns :: Namespace
ns = Namespace "hydra.test.inference.all"

module_ :: Module
module_ = Module ns elements namespaces kernelTypesNamespaces $
    Just "Hydra's inference test suite"
  where
    elements = [Phantoms.toBinding allTests]
    namespaces = [
      AlgebraicTypes.ns,
      AlgorithmW.ns,
      Failures.ns,
      Fundamentals.ns,
      KernelExamples.ns,
      NominalTypes.ns]

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all inference tests" $
    Testing.testGroup (string "inference") nothing (list subgroups) (list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = [
      AlgebraicTypes.allTests,
      AlgorithmW.allTests,
      Failures.allTests,
      Fundamentals.allTests,
      KernelExamples.allTests,
      NominalTypes.allTests]
