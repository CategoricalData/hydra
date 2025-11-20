{-# LANGUAGE OverloadedStrings #-}

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


module_ :: Module
module_ = Module (Namespace "hydra.test.inference.all") elements modules kernelTypesModules $
    Just "Hydra's inference test suite"
  where
    elements = [el allTestsDef]
    modules = [
      AlgebraicTypes.module_,
      AlgorithmW.module_,
      Failures.module_,
      Fundamentals.module_,
      KernelExamples.module_,
      NominalTypes.module_]

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    doc "The group of all inference tests" $
    Testing.testGroup "inference" nothing (list subgroups) (list [])
  where
    subgroups = [
      ref AlgebraicTypes.allTestsDef,
      ref AlgorithmW.allTestsDef,
      ref Failures.allTestsDef,
      ref Fundamentals.allTestsDef,
      ref KernelExamples.allTestsDef,
      ref NominalTypes.allTestsDef]
