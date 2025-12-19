module Hydra.Sources.Test.Lib.Pairs where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes
import qualified Hydra.Sources.Test.TestGraph as TestGraph


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.pairs") elements [] [] $
    Just "Test cases for hydra.lib.pairs primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Helper to create pair terms
pairTerm :: Int -> String -> TTerm Term
pairTerm i s = MetaTerms.pair (int32 i) (MetaTerms.string s)

-- Test groups for hydra.lib.pairs primitives

pairsBimap :: TTerm TestGroup
pairsBimap = subgroup "bimap" [
  test "transform both elements" 5 "ab" 10 2,
  test "with zero" 0 "hello" 0 5]
  where
    test name fst snd resultFst resultSnd = primCase name _pairs_bimap [
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      lambda "s" (primitive _strings_length @@ var "s"),
      pairTerm fst snd] (MetaTerms.pair (int32 resultFst) (int32 resultSnd))

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
    test name fst snd result = primCase name _pairs_second [pairTerm fst snd] (MetaTerms.string result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.pairs primitives" $
    supergroup "hydra.lib.pairs primitives" [
      pairsBimap,
      pairsFirst,
      pairsSecond]
