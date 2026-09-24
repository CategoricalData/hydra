
module Hydra.Sources.Test.Inference.All where

import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing as Testing
import Hydra.Sources.Kernel.Types.All
import Hydra.Core.Overlay.Haskell.Dsl.Phantoms as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M

import qualified Hydra.Sources.Test.Inference.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Sources.Test.Inference.AlgorithmW as AlgorithmW
import qualified Hydra.Sources.Test.Inference.Annotations as Annotations
import qualified Hydra.Sources.Test.Inference.Classes as Classes
import qualified Hydra.Sources.Test.Inference.Failures as Failures
import qualified Hydra.Sources.Test.Inference.Fundamentals as Fundamentals
import qualified Hydra.Sources.Test.Inference.Idempotence as Idempotence
import qualified Hydra.Sources.Test.Inference.KernelExamples as KernelExamples
import qualified Hydra.Sources.Test.Inference.NominalTypes as NominalTypes


ns :: ModuleName
ns = ModuleName "hydra.core.test.inference.all"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (namespaces Prelude.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Hydra's inference test suite")}
  where
    definitions = [Phantoms.toDefinition allTests]
    namespaces = [
      AlgebraicTypes.ns,
      AlgorithmW.ns,
      Annotations.ns,
      Classes.ns,
      Failures.ns,
      Fundamentals.ns,
      Idempotence.ns,
      KernelExamples.ns,
      NominalTypes.ns]

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all inference tests" $
    Testing.testGroup (string "inference") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [
      AlgebraicTypes.allTests,
      AlgorithmW.allTests,
      Annotations.allTests,
      Classes.allTests,
      Failures.allTests,
      Fundamentals.allTests,
      Idempotence.allTests,
      KernelExamples.allTests,
      NominalTypes.allTests]
