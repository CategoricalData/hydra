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
import qualified Hydra.Sources.Test.Lib.Chars as Chars
import qualified Hydra.Sources.Test.Lib.Eithers as Eithers
import qualified Hydra.Sources.Test.Lib.Equality as Equality
import qualified Hydra.Sources.Test.Lib.Flows as Flows
import qualified Hydra.Sources.Test.Lib.Lists as Lists
import qualified Hydra.Sources.Test.Lib.Literals as Literals
import qualified Hydra.Sources.Test.Lib.Logic as Logic
import qualified Hydra.Sources.Test.Lib.Maps as Maps
import qualified Hydra.Sources.Test.Lib.Math as Math
import qualified Hydra.Sources.Test.Lib.Maybes as Maybes
import qualified Hydra.Sources.Test.Lib.Pairs as Pairs
import qualified Hydra.Sources.Test.Lib.Sets as Sets
import qualified Hydra.Sources.Test.Lib.Strings as Strings
import qualified Hydra.Sources.Test.Checking.All as CheckingAll
import qualified Hydra.Sources.Test.EtaExpansion as EtaExpansion
import qualified Hydra.Sources.Test.Formatting as Formatting
import qualified Hydra.Sources.Test.Inference.All as InferenceAll
import qualified Hydra.Sources.Test.Json.Coder as JsonCoder
import qualified Hydra.Sources.Test.Json.Parser as JsonParser
import qualified Hydra.Sources.Test.Json.Writer as JsonWriter
import qualified Hydra.Sources.Test.Reduction as Reduction
import qualified Hydra.Sources.Test.Rewriting as Rewriting
import qualified Hydra.Sources.Test.Serialization as Serialization
import qualified Hydra.Sources.Test.Sorting as Sorting


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
      JsonCoder.module_,
      JsonParser.module_,
      JsonWriter.module_,
      Reduction.module_,
      Rewriting.module_,
      Serialization.module_,
      Sorting.module_,
      Chars.module_,
      Eithers.module_,
      Equality.module_,
      -- Flows.module_, -- Temporarily disabled: Flow tests cause type unification errors in generation
      Lists.module_,
      Literals.module_,
      Logic.module_,
      Maps.module_,
      Math.module_,
      Maybes.module_,
      Pairs.module_,
      Sets.module_,
      Strings.module_]

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
      ref JsonCoder.allTestsDef,
      ref JsonParser.allTestsDef,
      ref JsonWriter.allTestsDef,
      ref Reduction.allTestsDef,
      ref Rewriting.allTestsDef,
      ref Serialization.allTestsDef,
      ref Sorting.allTestsDef,
      ref Chars.allTestsDef,
      ref Eithers.allTestsDef,
      ref Equality.allTestsDef,
      -- ref Flows.allTestsDef, -- Temporarily disabled: Flow tests cause type unification errors in generation
      ref Lists.allTestsDef,
      ref Literals.allTestsDef,
      ref Logic.allTestsDef,
      ref Maps.allTestsDef,
      ref Math.allTestsDef,
      ref Maybes.allTestsDef,
      ref Pairs.allTestsDef,
      ref Sets.allTestsDef,
      ref Strings.allTestsDef]
