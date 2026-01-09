
module Hydra.Sources.Test.TestSuite where

import Hydra.Kernel
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Sources.Kernel.Types.All
import Hydra.Dsl.Meta.Phantoms as Phantoms hiding ((++))
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
import qualified Hydra.Sources.Test.Annotations as Annotations
import qualified Hydra.Sources.Test.Monads as Monads
import qualified Hydra.Sources.Test.Lib.Pairs as Pairs
import qualified Hydra.Sources.Test.Lib.Sets as Sets
import qualified Hydra.Sources.Test.Lib.Strings as Strings
import qualified Hydra.Sources.Test.Checking.All as CheckingAll
import qualified Hydra.Sources.Test.Checking.Advanced as CheckingAdvanced
import qualified Hydra.Sources.Test.Checking.AlgebraicTypes as CheckingAlgebraicTypes
import qualified Hydra.Sources.Test.Checking.Collections as CheckingCollections
import qualified Hydra.Sources.Test.Checking.Failures as CheckingFailures
import qualified Hydra.Sources.Test.Checking.Fundamentals as CheckingFundamentals
import qualified Hydra.Sources.Test.Checking.NominalTypes as CheckingNominalTypes
import qualified Hydra.Sources.Test.EtaExpansion as EtaExpansion
import qualified Hydra.Sources.Test.Formatting as Formatting
import qualified Hydra.Sources.Test.Inference.All as InferenceAll
import qualified Hydra.Sources.Test.Inference.AlgebraicTypes as InferenceAlgebraicTypes
import qualified Hydra.Sources.Test.Inference.AlgorithmW as InferenceAlgorithmW
import qualified Hydra.Sources.Test.Inference.Failures as InferenceFailures
import qualified Hydra.Sources.Test.Inference.Fundamentals as InferenceFundamentals
import qualified Hydra.Sources.Test.Inference.KernelExamples as InferenceKernelExamples
import qualified Hydra.Sources.Test.Inference.NominalTypes as InferenceNominalTypes
import qualified Hydra.Sources.Test.Json.Coder as JsonCoder
import qualified Hydra.Sources.Test.Json.Parser as JsonParser
import qualified Hydra.Sources.Test.Json.Roundtrip as JsonRoundtrip
import qualified Hydra.Sources.Test.Json.Writer as JsonWriter
import qualified Hydra.Sources.Test.Hoisting as Hoisting
import qualified Hydra.Sources.Test.Reduction as Reduction
import qualified Hydra.Sources.Test.Rewriting as Rewriting
import qualified Hydra.Sources.Test.Serialization as Serialization
import qualified Hydra.Sources.Test.Sorting as Sorting


ns :: Namespace
ns = Namespace "hydra.test.testSuite"

module_ :: Module
module_ = Module ns elements namespaces kernelTypesNamespaces $
    Just ("Hydra's common test suite, which is designed to run identically in each Hydra implementation;"
      <> " the criterion for a true Hydra implementation is that all test cases pass.")
  where
    elements = [Phantoms.toBinding allTests]
    namespaces = fst <$> testPairs

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all common tests" $
    Testing.testGroup (string "common") nothing (list subgroups) (list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = snd <$> testPairs

libPairs :: [(Namespace, TBinding TestGroup)]
libPairs = [
  (Chars.ns, Chars.allTests),
  (Eithers.ns, Eithers.allTests),
  (Equality.ns, Equality.allTests),
  (Flows.ns, Flows.allTests),
  (Lists.ns, Lists.allTests),
  (Literals.ns, Literals.allTests),
  (Logic.ns, Logic.allTests),
  (Maps.ns, Maps.allTests),
  (Math.ns, Math.allTests),
  (Maybes.ns, Maybes.allTests),
  (Pairs.ns, Pairs.allTests),
  (Sets.ns, Sets.allTests),
  (Strings.ns, Strings.allTests)]

otherPairs :: [(Namespace, TBinding TestGroup)]
otherPairs = [
  (Annotations.ns, Annotations.allTests),
  (CheckingAll.ns, CheckingAll.allTests),
  (EtaExpansion.ns, EtaExpansion.allTests),
  (Formatting.ns, Formatting.allTests),
  (Hoisting.ns, Hoisting.allTests),
  (InferenceAll.ns, InferenceAll.allTests),
  (JsonCoder.ns, JsonCoder.allTests),
  (JsonParser.ns, JsonParser.allTests),
  (JsonRoundtrip.ns, JsonRoundtrip.allTests),
  (JsonWriter.ns, JsonWriter.allTests),
  (Monads.ns, Monads.allTests),
  (Reduction.ns, Reduction.allTests),
  (Rewriting.ns, Rewriting.allTests),
  (Serialization.ns, Serialization.allTests),
  (Sorting.ns, Sorting.allTests)]

testPairs :: [(Namespace, TBinding TestGroup)]
testPairs = libPairs ++ otherPairs

-- | All test suite modules (the actual Module values)
testSuiteModules :: [Module]
testSuiteModules =
  -- Lib tests
  [Chars.module_, Eithers.module_, Equality.module_, Flows.module_,
   Lists.module_, Literals.module_, Logic.module_, Maps.module_,
   Math.module_, Maybes.module_, Pairs.module_, Sets.module_, Strings.module_,
   -- Other tests
   Annotations.module_, EtaExpansion.module_, Formatting.module_, Hoisting.module_,
   JsonCoder.module_, JsonParser.module_, JsonRoundtrip.module_, JsonWriter.module_,
   Monads.module_, Reduction.module_, Rewriting.module_, Serialization.module_, Sorting.module_,
   -- Checking tests (including sub-modules)
   CheckingAll.module_,
   CheckingAdvanced.module_, CheckingAlgebraicTypes.module_, CheckingCollections.module_,
   CheckingFailures.module_, CheckingFundamentals.module_, CheckingNominalTypes.module_,
   -- Inference tests (including sub-modules)
   InferenceAll.module_,
   InferenceAlgebraicTypes.module_, InferenceAlgorithmW.module_, InferenceFailures.module_,
   InferenceFundamentals.module_, InferenceKernelExamples.module_, InferenceNominalTypes.module_]
