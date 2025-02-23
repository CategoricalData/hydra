module Hydra.Sources.Tier3.Test.Inference.Fundamentals (fundamentalsTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph

import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


fundamentalsTests :: TTerm TestGroup
fundamentalsTests = supergroup "Fundamentals" [
  testGroupForLambdas,
  testGroupForLet,
  testGroupForLiterals,
  testGroupForPathologicalTerms,
  testGroupForPolymorphism,
  testGroupForPrimitives]

testGroupForLambdas :: TTerm TestGroup
testGroupForLambdas = subgroup "Lambdas" [
    expectPoly 1 []
      (lambda "x" $ var "x")
      ["t0"] (T.function (T.var "t0") (T.var "t0")),
    expectPoly 2 []
      (lambda "x" $ int16 137)
      ["t0"] (T.function (T.var "t0") T.int16)]

testGroupForLet :: TTerm TestGroup
testGroupForLet = supergroup "Let" [

    subgroup "Simple" [
      expectPoly 1  []
        (let1 "x" (float32 42.0) (lambda "y" (lambda "z" (var "x"))))
        ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") T.float32))],
    subgroup "Empty let" [
      expectMono 1 [tag_disabledForAltInference]
        (lets [] $ int32 42)
        T.int32],
    subgroup "Trivial let" [
      expectMono 1  []
        (lets [
          "foo">: int32 42]
          $ var "foo")
        T.int32],
    subgroup "Multiple references to a let-bound term" [
      expectMono 1 [tag_disabledForAltInference]
        (lets [
          "foo">: int32 42,
          "bar">: int32 137]
          $ list [var "foo", var "bar", var "foo"])
        (T.list T.int32)],

    subgroup "Let-polymorphism" [
      expectPoly 1 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ lambda "x" $ var "id" @@ (var "id" @@ var "x"))
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectMono 2 [tag_disabled]
        (lets [
          "id">: lambda "x" $ var "x"]
          $ var "id" @@ (list [var "id" @@ int32 42]))
        (T.list T.int32),
      expectPoly 3 [tag_disabled]
        (lets [
          "id">: lambda "x" $ var "x"]
          $ lambda "x" (var "id" @@ (list [var "id" @@ var "x"])))
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectMono 4 [tag_disabled]
        (lets [
          "id">: lambda "x" $ var "x"]
          $ pair (var "id" @@ int32 42) (var "id" @@ string "foo"))
        (T.pair T.int32 T.string),
      expectMono 5 [tag_disabled]
        (lets [
          "list">: lambda "x" $ list [var "x"]]
          $ pair (var "list" @@ int32 42) (var "list" @@ string "foo"))
        (T.pair (T.list T.int32) (T.list T.string)),
      expectPoly 6 [tag_disabled]
        (lets [
          "singleton">: lambda "x" $ list [var "x"],
          "f">: lambda "x" $ lambda "y" $ primitive _lists_cons
            @@ (pair (var "singleton" @@ var "x") (var "singleton" @@ var "y"))
            @@ (var "g" @@ var "x" @@ var "y"),
          "g">: lambda "x" $ lambda "y" $ var "f" @@ int32 42 @@ var "y"]
          $ var "f")
        ["t0"] (T.list $ T.pair T.int32 (T.var "t0"))],

--  H.describe "Recursive and mutually recursive let (@wisnesky's test cases)" $ do
    subgroup "Recursive and mutually recursive let (@wisnesky's test cases)" []]
--      expectPoly 1
--        (lets [
--          "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")]
--          $ var "f")
--        ["t0"] (T.function T.int32 (T.function T.int32 (T.var "t0"))),
--      expectPoly 2
--        (lets [
--          "f">: var "g",
--          "g">: var "f"]
--          $ pair (var "f") (var "g"))
--        -- Note: GHC finds (a, b) rather than (a, a)
--        -- Try: :t (let (f, g) = (g, f) in (f, g))
--        ["t0"] (T.pair (T.var "t0") (T.var "t0")),
--      expectPoly 3
--        (lets [
--          "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
--          "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
--          $ pair (var "f") (var "g"))
--        ["t0", "t1"] (T.pair
--          (T.function (T.var "v0") (T.function T.int32 (T.var "t1")))
--          (T.function T.int32 (T.function (T.var "v0") (T.var "t1")))),
--      expectPoly 4
--        -- letrec + = (\x . (\y . (S (+ (P x) y)))) in (+ (S (S 0)) (S 0))
--        (lets [
--          "plus">: lambda "x" $ lambda "y" (s @@ (var "plus" @@ (p @@ var "x") @@ var "y"))]
--          $ var "plus" @@ (s @@ (s @@ int32 0)) @@ (s @@ int32 0))
--        ["t0"] (T.function T.int32 $ T.function (T.var "t0") T.int32),
--      expectMono 5
--        -- letrecs id = (\z. z)
--        --     f = (\p0. (pair (id p0) (id p0)))
--        --     in 0
--        (lets [
--          "id">: lambda "z" $ var "z",
--          "f">: lambda "p0" $ pair (var "id" @@ var "p0") (var "id" @@ var "p0")]
--          $ int32 0)
--        T.int32]]
  where
    s = primitive _math_neg
    p = primitive _math_neg

testGroupForLiterals :: TTerm TestGroup
testGroupForLiterals = subgroup "Literals" [
    expectMono 1 []
      (int32 42)
      T.int32,
    expectMono 2 []
      (string "foo")
      T.string,
    expectMono 3 []
      false
      T.boolean,
    expectMono 4 []
      (float64 42.0)
      T.float64]

testGroupForPathologicalTerms :: TTerm TestGroup
testGroupForPathologicalTerms = supergroup "Pathological terms" [

    subgroup "Infinite lists" [
      expectMono 1 [tag_disabled]
        (lets [
          "self">: primitive _lists_cons @@ int32 42 @@ var "self"]
          $ var "self")
        (T.list T.int32),
      expectPoly 2  [tag_disabled]
        (lambda "x" $ lets [
          "self">: primitive _lists_cons @@ var "x" @@ var "self"]
          $ var "self")
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectPoly 3  [tag_disabled]
        (lets [
          "self">: lambda "e" $ primitive _lists_cons @@ var "e" @@ (var "self" @@ var "e")]
          $ lambda "x" $ var "self" @@ var "x")
        ["t0"] (T.function T.int32 (T.list T.int32)),
      expectMono 4  [tag_disabled]
        (lets [
          "build">: lambda "x" $ primitive _lists_cons @@ var "x" @@ (var "build" @@
            (primitive _math_add @@ var "x" @@ int32 1))]
          $ var "build" @@ int32 0)
        (T.list T.int32)]]
  -- TODO: this term *should* fail inference, but doesn't
--    H.it "Check self-application" $ do
--      expectFailure
--        (lambda "x" $ var "x" @@ var "x")

testGroupForPolymorphism :: TTerm TestGroup
testGroupForPolymorphism = supergroup "Polymorphism" [

    subgroup "Simple lists and optionals" [
      expectPoly 1 []
        (list [])
        ["t0"] (T.list (T.var "t0")),
      expectPoly 2 [tag_disabledForAlgorithmWInference, tag_disabledForAltInference]
        (optional nothing)
        ["t0"] (T.optional (T.var "t0")),
      expectMono 3 [tag_disabledForAlgorithmWInference, tag_disabledForAltInference]
        (optional $ just $ int32 42)
        (T.optional T.int32)],

    subgroup "Lambdas, lists, and products" [
      expectPoly 1 []
        (lambda "x" $ var "x")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectPoly 2 []
        (lambda "x" $ pair (var "x") (var "x"))
        ["t0"] (T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t0"))),
      expectPoly 3 []
        (lambda "x" $ list [var "x"])
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectPoly 4 []
        (list [lambda "x" $ var "x", lambda "y" $ var "y"])
        ["t0"] (T.list (T.function (T.var "t0") (T.var "t0"))),
      expectPoly 5 []
        (list [lambda "x" $ lambda "y" $ pair (var "y") (var "x")])
        ["t0", "t1"] (T.list (T.function (T.var "t0") (T.function (T.var "t1") (T.pair (T.var "t1") (T.var "t0")))))],

    subgroup "Lambdas and application" [
      expectMono 1 []
        ((lambda "x" $ var "x") @@ string "foo")
        T.string],

    subgroup "Primitives and application" [
      expectMono 1 [tag_disabledForAltInference]
        (primitive _lists_concat @@ list [list [int32 42], list []])
        (T.list T.int32)],

    subgroup "Lambdas and primitives" [
      expectMono 1 [tag_disabledForAltInference]
        (primitive _math_add)
        (T.functionN [T.int32, T.int32, T.int32]),
      expectMono 2 [tag_disabledForAltInference]
        (lambda "x" (primitive _math_add @@ var "x"))
        (T.functionN [T.int32, T.int32, T.int32]),
      expectMono 3 [tag_disabledForAltInference]
        (lambda "x" (primitive _math_add @@ var "x" @@ var "x"))
        (T.function T.int32 T.int32)],

    subgroup "Mixed expressions with lambdas, constants, and primitive functions" [
      expectMono 1 [tag_disabledForAltInference]
        (lambda "x" $ (primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1))
        (T.function T.int32 T.int32)]]

testGroupForPrimitives :: TTerm TestGroup
testGroupForPrimitives = supergroup "Primitives" [

    subgroup "Monomorphic primitive functions" [
      expectMono 1 [tag_disabledForAltInference]
        (primitive $ _strings_length)
        (T.function T.string T.int32),
      expectMono 2 [tag_disabledForAltInference]
        (primitive _math_sub)
        (T.functionN [T.int32, T.int32, T.int32])],

    subgroup "Polymorphic primitive functions" [
      expectPoly 1 [tag_disabledForAltInference]
        (lambda "el" (primitive _lists_length @@ (list [var "el"])))
        ["t0"] (T.function (T.var "t0") T.int32),
      expectMono 2 [tag_disabledForAltInference]
        (lambda "el" (primitive _lists_length @@ (list [int32 42, var "el"])))
        (T.function T.int32 T.int32),
      expectPoly 3 [tag_disabledForAltInference]
        (primitive _lists_concat)
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") (T.list $ T.var "t0")),
      expectPoly 4 [tag_disabledForAltInference]
        (lambda "lists" (primitive _lists_concat @@ var "lists"))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") (T.list $ T.var "t0")),
      expectPoly 5 [tag_disabledForAltInference]
        (lambda "lists" (primitive _lists_length @@ (primitive _lists_concat @@ var "lists")))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") T.int32),
      expectPoly 6 [tag_disabledForAltInference]
        (lambda "list" (primitive _lists_length @@ (primitive _lists_concat @@ list [var "list", list []])))
        ["t0"] (T.function (T.list $ T.var "t0") T.int32),
      expectPoly 7 [tag_disabledForAltInference]
        (lambda "list" (primitive _math_add
          @@ int32 1
          @@ (primitive _lists_length @@ (primitive _lists_concat @@ list [var "list", list []]))))
        ["t0"] (T.function (T.list $ T.var "t0") T.int32),
      expectPoly 8 [tag_disabledForAltInference]
        (lambda "lists" (primitive _lists_length @@ (primitive _lists_concat @@ var "lists")))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") T.int32)]]