
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
    elements = [Phantoms.toBinding allTests]
    modules = fst <$> testPairs

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all common tests" $
    Testing.testGroup (string "common") nothing (list subgroups) (list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = snd <$> testPairs

libPairs :: [(Module, TBinding TestGroup)]
libPairs = [
  (Chars.module_, Chars.allTests),
  (Eithers.module_, Eithers.allTests),
  (Equality.module_, Equality.allTests),
  (Flows.module_, Flows.allTests),
  (Lists.module_, Lists.allTests),
  (Literals.module_, Literals.allTests),
  (Logic.module_, Logic.allTests),
  (Maps.module_, Maps.allTests),
  (Math.module_, Math.allTests),
  (Maybes.module_, Maybes.allTests),
  (Pairs.module_, Pairs.allTests),
  (Sets.module_, Sets.allTests),
  (Strings.module_, Strings.allTests)]

otherPairs :: [(Module, TBinding TestGroup)]
otherPairs = [
  (Annotations.module_, Annotations.allTests),
  (CheckingAll.module_, CheckingAll.allTests),
  (EtaExpansion.module_, EtaExpansion.allTests),
  (Formatting.module_, Formatting.allTests),
  (InferenceAll.module_, InferenceAll.allTests),
  (JsonCoder.module_, JsonCoder.allTests),
  (JsonParser.module_, JsonParser.allTests),
  (JsonWriter.module_, JsonWriter.allTests),
  (Monads.module_, Monads.allTests),
  (Reduction.module_, Reduction.allTests),
  (Rewriting.module_, Rewriting.allTests),
  (Serialization.module_, Serialization.allTests),
  (Sorting.module_, Sorting.allTests)]

testPairs :: [(Module, TBinding TestGroup)]
testPairs = libPairs ++ otherPairs
