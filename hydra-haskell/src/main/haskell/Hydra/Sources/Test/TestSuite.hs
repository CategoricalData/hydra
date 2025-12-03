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
import qualified Hydra.Sources.Test.Monads as Monads
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
    modules = fst <$> testPairs

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    doc "The group of all common tests" $
    Testing.testGroup "common" nothing (list subgroups) (list [])
  where
    subgroups = snd <$> testPairs

libPairs :: [(Module, TTerm TestGroup)]
libPairs = [
  (Chars.module_, ref Chars.allTestsDef),
  (Eithers.module_, ref Eithers.allTestsDef),
  (Equality.module_, ref Equality.allTestsDef),
  -- (Flows.module_, ref Flows.allTestsDef), -- Temporarily disabled: Flow tests cause type unification errors in generation
  (Lists.module_, ref Lists.allTestsDef),
  (Literals.module_, ref Literals.allTestsDef),
  (Logic.module_, ref Logic.allTestsDef),
  (Maps.module_, ref Maps.allTestsDef),
  (Math.module_, ref Math.allTestsDef),
  (Maybes.module_, ref Maybes.allTestsDef),
  (Pairs.module_, ref Pairs.allTestsDef),
  (Sets.module_, ref Sets.allTestsDef),
  (Strings.module_, ref Strings.allTestsDef)]

otherPairs :: [(Module, TTerm TestGroup)]
otherPairs = [
  (CheckingAll.module_, ref CheckingAll.allTestsDef),
  (EtaExpansion.module_, ref EtaExpansion.allTestsDef),
  (Formatting.module_, ref Formatting.allTestsDef),
  (InferenceAll.module_, ref InferenceAll.allTestsDef),
  (JsonCoder.module_, ref JsonCoder.allTestsDef),
  (JsonParser.module_, ref JsonParser.allTestsDef),
  (JsonWriter.module_, ref JsonWriter.allTestsDef),
  (Monads.module_, ref Monads.allTestsDef),
  (Reduction.module_, ref Reduction.allTestsDef),
  (Rewriting.module_, ref Rewriting.allTestsDef),
  (Serialization.module_, ref Serialization.allTestsDef),
  (Sorting.module_, ref Sorting.allTestsDef)]

testPairs :: [(Module, TTerm TestGroup)]
testPairs = libPairs ++ otherPairs
