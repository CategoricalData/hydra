module Hydra.Sources.Test.Lib.Pairs where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.lib.pairs"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for hydra.lib.pairs primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Helper to create pair terms
pairTerm :: Int -> String -> TTerm Term
pairTerm i s = Terms.pair (int32 i) (Terms.string s)

-- Test groups for hydra.lib.pairs primitives

pairsBimap :: TTerm TestGroup
pairsBimap = subgroup "bimap" [
  test "transform both elements" 5 "ab" 10 2,
  test "with zero" 0 "hello" 0 5]
  where
    test name fst snd resultFst resultSnd = primCase name _pairs_bimap [
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      lambda "s" (primitive _strings_length @@ var "s"),
      pairTerm fst snd] (Terms.pair (int32 resultFst) (int32 resultSnd))

pairsFirst :: TTerm TestGroup
pairsFirst = subgroup "first" [
  test "extract first element" 42 "hello" 42,
  test "with zero" 0 "world" 0,
  test "negative number" (-5) "test" (-5)]
  where
    test name fst snd result = primCase name _pairs_first [pairTerm fst snd] (int32 result)

pairsSecond :: TTerm TestGroup
pairsSecond = subgroup "second" [
  test "extract second element" 42 "hello" "hello",
  test "empty string" 0 "" "",
  test "long string" 123 "testing" "testing"]
  where
    test name fst snd result = primCase name _pairs_second [pairTerm fst snd] (Terms.string result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.pairs primitives" $
    supergroup "hydra.lib.pairs primitives" [
      pairsBimap,
      pairsFirst,
      pairsSecond]
