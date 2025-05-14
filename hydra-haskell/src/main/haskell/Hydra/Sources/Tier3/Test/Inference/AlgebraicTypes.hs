module Hydra.Sources.Tier3.Test.Inference.AlgebraicTypes (algebraicTypesTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Tier3.Test.Inference.Fundamentals

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


algebraicTypesTests :: TTerm TestGroup
algebraicTypesTests = supergroup "Algebraic terms" [
  testGroupForFolds,
  testGroupForLists,
  testGroupForMaps,
  testGroupForOptionals,
  testGroupForProducts,
  testGroupForSets,
  testGroupForSums]

testGroupForFolds :: TTerm TestGroup
testGroupForFolds = subgroup "List eliminations (folds)" [
    expectMono 1 [tag_disabledForMinimalInference]
      foldAdd
      (T.functionMany [T.int32, T.list T.int32, T.int32]),
    expectMono 2 [tag_disabledForMinimalInference]
      (foldAdd @@ int32 0)
      (T.function (T.list T.int32) T.int32),
    expectMono 3 [tag_disabledForMinimalInference]
      (foldAdd @@ int32 0 @@ (list (int32 <$> [1, 2, 3, 4, 5])))
      T.int32]
  where
    foldAdd = primitive _lists_foldl @@ primitive _math_add

testGroupForLists :: TTerm TestGroup
testGroupForLists = supergroup "List terms" [
    subgroup "List of strings" [
      expectMono 1 []
        (list [string "foo", string "bar"])
        (T.list T.string)],
    subgroup "List of lists of strings" [
      expectMono 1 []
        (list [list [string "foo"], list []])
        (T.list $ T.list T.string)],
    subgroup "Empty list" [
      expectPoly 1 []
        (list [])
        ["t0"] (T.list $ T.var "t0")],
    subgroup "List containing an empty list" [
      expectPoly 1 []
        (list [list []])
        ["t0"] (T.list $ T.list $ T.var "t0")],
    subgroup "Lambda producing a polymorphic list" [
      expectPoly 1 []
        (lambda "x" (list [var "x"]))
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0"))],
    subgroup "Lambda producing a list of integers" [
      expectMono 1 []
        (lambda "x" (list [var "x", int32 42]))
        (T.function T.int32 $ T.list T.int32)],
    subgroup "List with repeated variables" [
      expectMono 1 []
        (lambda "x" (list [var "x", string "foo", var "x"]))
        (T.function T.string (T.list T.string))]]

testGroupForMaps :: TTerm TestGroup
testGroupForMaps = subgroup "Map terms" [
    expectMono 1 [tag_disabledForMinimalInference]
      (mapTermCheat [
        (Terms.string "firstName", Terms.string "Arthur"),
        (Terms.string "lastName", Terms.string "Dent")])
      (T.map T.string T.string),
    expectPoly 2 [tag_disabledForMinimalInference]
      (mapTermCheat [])
      ["t0", "t1"] (T.map (T.var "t0") (T.var "t1"))]

testGroupForOptionals :: TTerm TestGroup
testGroupForOptionals = subgroup "Optional terms" [
    expectMono 1 [tag_disabledForMinimalInference]
      (optional $ just $ int32 42)
      (T.optional T.int32),
    expectPoly 2 [tag_disabledForMinimalInference]
      (optional nothing)
      ["t0"] (T.optional $ T.var "t0")]

testGroupForProducts :: TTerm TestGroup
testGroupForProducts = supergroup "Product terms" [
    subgroup "Empty products" [
      expectMono 1 []
        (product [])
        (T.product [])],

    subgroup "Non-empty, monotyped products" [
      expectMono 1 []
        (product [string "foo", int32 42])
        (T.product [T.string, T.int32]),
      expectMono 2 []
        (product [string "foo", list [float32 42.0, float32 137.0]])
        (T.product [T.string, T.list T.float32]),
      expectMono 3 [tag_disabledForMinimalInference]
        (product [string "foo", int32 42, list [float32 42.0, float32 137.0]])
        (T.product [T.string, T.int32, T.list T.float32])],

    subgroup "Polytyped products" [
      expectPoly 1 []
        (product [list [], string "foo"])
        ["t0"] (T.product [T.list $ T.var "t0", T.string]),
      expectPoly 2 [tag_disabledForMinimalInference]
        (product [int32 42, string "foo", list []])
        ["t0"] (T.product [T.int32, T.string, T.list $ T.var "t0"])],

    subgroup "Pairs" [
      expectMono 1 []
        (pair (int32 42) (string "foo"))
        (T.pair T.int32 T.string),
      expectPoly 2 []
        (pair (list []) (string "foo"))
        ["t0"] (T.pair (T.list $ T.var "t0") T.string),
      expectPoly 3 []
        (pair (list []) (list []))
        ["t0", "t1"] (T.pair (T.list $ T.var "t0") (T.list $ T.var "t1"))]]

testGroupForSets :: TTerm TestGroup
testGroupForSets = subgroup "Set terms" [
    expectMono 1 [tag_disabledForMinimalInference]
      (set [true])
      (T.set T.boolean),
    expectPoly 2 [tag_disabledForMinimalInference]
      (set [set []])
      ["t0"] (T.set $ T.set $ T.var "t0")]

testGroupForSums :: TTerm TestGroup
testGroupForSums = supergroup "Sum terms" [
    subgroup "Singleton sum terms" [
      expectMono 1 [tag_disabledForMinimalInference]
        (sum 0 1 (string "foo"))
        (T.sum [T.string]),
      expectPoly 2 [tag_disabledForMinimalInference]
        (sum 0 1 (list []))
        ["t0"] (T.sum [T.list $ T.var "t0"])],

    subgroup "Non-singleton sum terms" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (sum 0 2 (string "foo"))
        ["t0"] (T.sum [T.string, T.var "t0"]),
      expectPoly 2 [tag_disabledForMinimalInference]
        (sum 1 2 (string "foo"))
        ["t0"] (T.sum [T.var "t0", T.string])]]
