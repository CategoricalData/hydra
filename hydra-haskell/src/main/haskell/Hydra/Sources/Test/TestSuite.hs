{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Test.TestSuite where

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

-- Additional imports
import qualified Hydra.Sources.Test.Lib.Lists as Lists
import qualified Hydra.Sources.Test.Lib.Logic as Logic
import qualified Hydra.Sources.Test.Lib.Math as Math
import qualified Hydra.Sources.Test.Lib.Strings as Strings
import qualified Hydra.Sources.Test.Checking.All as CheckingAll
import qualified Hydra.Sources.Test.EtaExpansion as EtaExpansion
import qualified Hydra.Sources.Test.Formatting as Formatting
import qualified Hydra.Sources.Test.Inference.All as InferenceAll


module_ :: Module
module_ = Module (Namespace "hydra.test.testSuite") elements modules kernelTypesModules $
    Just ("Hydra's common test suite, which is designed to run identically in each Hydra implementation;"
      <> " the criterion for a true Hydra implementation is that all test cases pass.")
  where
    elements = [el allTestsDef]
    modules = [
      CheckingAll.module_,
      EtaExpansion.module_,
      Formatting.module_,
      InferenceAll.module_,
      Lists.module_, Logic.module_, Math.module_, Strings.module_]

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    doc "The group of all common tests" $
    Testing.testGroup "common" nothing (list subgroups) (list [])
  where
    subgroups = [
      ref CheckingAll.allTestsDef,
      ref EtaExpansion.allTestsDef,
      ref Formatting.allTestsDef,
      ref InferenceAll.allTestsDef,
      ref Lists.allTestsDef,
      ref Logic.allTestsDef,
      ref Math.allTestsDef,
      ref Strings.allTestsDef]
