{-# LANGUAGE FlexibleContexts #-}

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
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs as Pairs
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings


ns :: Namespace
ns = Namespace "hydra.test.lib.pairs"

module_ :: Module
module_ = Module ns elements
    [Namespace "hydra.reduction", Namespace "hydra.show.core"]
    kernelTypesNamespaces $
    Just "Test cases for hydra.lib.pairs primitives"
  where
    elements = [Phantoms.toDefinition allTests]

showInt32 :: TTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

-- Show a (Int, Int) pair as "(<int>, <int>)"
showIntIntPair :: TTerm ((Int, Int) -> String)
showIntIntPair = Phantoms.lambda "p" $ Strings.cat (Phantoms.list [
  Phantoms.string "(",
  Literals.showInt32 (Pairs.first (Phantoms.var "p")),
  Phantoms.string ", ",
  Literals.showInt32 (Pairs.second (Phantoms.var "p")),
  Phantoms.string ")"])

-- Test groups for hydra.lib.pairs primitives

pairsBimap :: TTerm TestGroup
pairsBimap = subgroup "bimap" [
  test "transform both elements" 5 "ab" 10 2,
  test "with zero" 0 "hello" 0 5]
  where
    test name fst snd resultFst resultSnd = evalPair name showIntIntPair
      (Pairs.bimap
        (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2))
        (Phantoms.lambda "s" $ Strings.length (Phantoms.var "s"))
        (Phantoms.pair (Phantoms.int32 fst) (Phantoms.string snd)))
      (Phantoms.pair (Phantoms.int32 resultFst) (Phantoms.int32 resultSnd))

pairsFirst :: TTerm TestGroup
pairsFirst = subgroup "first" [
  test "extract first element" 42 "hello" 42,
  test "with zero" 0 "world" 0,
  test "negative number" (-5) "test" (-5)]
  where
    test name fst snd result = evalPair name showInt32
      (Pairs.first (Phantoms.pair (Phantoms.int32 fst) (Phantoms.string snd)))
      (Phantoms.int32 result)

pairsSecond :: TTerm TestGroup
pairsSecond = subgroup "second" [
  test "extract second element" 42 "hello" "hello",
  test "empty string" 0 "" "",
  test "long string" 123 "testing" "testing"]
  where
    test name fst snd result = stringEvalPair name
      (Pairs.second (Phantoms.pair (Phantoms.int32 fst) (Phantoms.string snd)))
      (Phantoms.string result)

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.pairs primitives" $
    supergroup "hydra.lib.pairs primitives" [
      pairsBimap,
      pairsFirst,
      pairsSecond]
