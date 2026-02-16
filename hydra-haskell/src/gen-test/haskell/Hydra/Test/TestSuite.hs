-- Note: this is an automatically generated file. Do not edit.

-- | Hydra's common test suite, which is designed to run identically in each Hydra implementation; the criterion for a true Hydra implementation is that all test cases pass.

module Hydra.Test.TestSuite where

import qualified Hydra.Test.Annotations as Annotations
import qualified Hydra.Test.Checking.All as All
import qualified Hydra.Test.EtaExpansion as EtaExpansion
import qualified Hydra.Test.Formatting as Formatting
import qualified Hydra.Test.Hoisting as Hoisting
import qualified Hydra.Test.Inference.All as All_
import qualified Hydra.Test.Json.Coder as Coder
import qualified Hydra.Test.Json.Parser as Parser
import qualified Hydra.Test.Json.Roundtrip as Roundtrip
import qualified Hydra.Test.Json.Writer as Writer
import qualified Hydra.Test.Lib.Chars as Chars
import qualified Hydra.Test.Lib.Eithers as Eithers
import qualified Hydra.Test.Lib.Equality as Equality
import qualified Hydra.Test.Lib.Flows as Flows
import qualified Hydra.Test.Lib.Lists as Lists
import qualified Hydra.Test.Lib.Literals as Literals
import qualified Hydra.Test.Lib.Logic as Logic
import qualified Hydra.Test.Lib.Maps as Maps
import qualified Hydra.Test.Lib.Math as Math
import qualified Hydra.Test.Lib.Maybes as Maybes
import qualified Hydra.Test.Lib.Pairs as Pairs
import qualified Hydra.Test.Lib.Sets as Sets
import qualified Hydra.Test.Lib.Strings as Strings
import qualified Hydra.Test.Monads as Monads
import qualified Hydra.Test.Reduction as Reduction
import qualified Hydra.Test.Rewriting as Rewriting
import qualified Hydra.Test.Serialization as Serialization
import qualified Hydra.Test.Sorting as Sorting
import qualified Hydra.Test.Substitution as Substitution
import qualified Hydra.Test.Unification as Unification
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The group of all common tests
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "common",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Chars.allTests,
    Eithers.allTests,
    Equality.allTests,
    Flows.allTests,
    Lists.allTests,
    Literals.allTests,
    Logic.allTests,
    Maps.allTests,
    Math.allTests,
    Maybes.allTests,
    Pairs.allTests,
    Sets.allTests,
    Strings.allTests,
    Annotations.allTests,
    All.allTests,
    EtaExpansion.allTests,
    Formatting.allTests,
    Hoisting.allTests,
    All_.allTests,
    Coder.allTests,
    Parser.allTests,
    Roundtrip.allTests,
    Writer.allTests,
    Monads.allTests,
    Reduction.allTests,
    Rewriting.allTests,
    Serialization.allTests,
    Sorting.allTests,
    Substitution.allTests,
    Unification.allTests],
  Testing.testGroupCases = []}
