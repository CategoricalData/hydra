{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Test.Checking.All where

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

import qualified Hydra.Sources.Test.Checking.Advanced as Advanced
import qualified Hydra.Sources.Test.Checking.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Sources.Test.Checking.Collections as Collections
import qualified Hydra.Sources.Test.Checking.Failures as Failures
import qualified Hydra.Sources.Test.Checking.Fundamentals as Fundamentals
import qualified Hydra.Sources.Test.Checking.NominalTypes as NominalTypes


module_ :: Module
module_ = Module (Namespace "hydra.test.checking.all") elements modules kernelTypesModules $
    Just "Hydra's type checking test suite"
  where
    elements = [el allTestsDef]
    modules = [
      Advanced.module_,
      AlgebraicTypes.module_,
      Collections.module_,
      Failures.module_,
      Fundamentals.module_,
      NominalTypes.module_]

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    doc "The group of all type checking tests" $
    Testing.testGroup "checking" nothing (list subgroups) (list [])
  where
    subgroups = [
      ref Advanced.allTestsDef,
      ref AlgebraicTypes.allTestsDef,
      ref Collections.allTestsDef,
      ref Failures.allTestsDef,
      ref Fundamentals.allTestsDef,
      ref NominalTypes.allTestsDef]
