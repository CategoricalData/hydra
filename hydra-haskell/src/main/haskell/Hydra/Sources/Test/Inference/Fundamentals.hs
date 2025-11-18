module Hydra.Sources.Test.Inference.Fundamentals (fundamentalsTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Dsl.Meta.Testing as Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Test.TestGraph

import           Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T

import qualified Data.Map as M
import Prelude hiding (map, sum)


fundamentalsTests :: TTerm TestGroup
fundamentalsTests = supergroup "Fundamentals" [
  testGroupForLambdas,
  testGroupForLet,
  testGroupForLiterals,
  testGroupForPathologicalTerms,
  testGroupForPolymorphism,
  testGroupForPrimitives]

testGroupForLambdas :: TTerm TestGroup
testGroupForLambdas = supergroup "Lambdas" [
    subgroup "Simple lambdas" [
      expectPoly 1 []
        (lambda "x" $ var "x")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectPoly 2 []
        (lambda "x" $ int16 137)
        ["t0"] (T.function (T.var "t0") T.int16)],

    subgroup "Nested lambdas" [
      expectMono 1 []
        (lambda "x" $ lambda "y" $ primitive _math_add @@ var "x" @@ var "y")
        (T.functionMany [T.int32, T.int32, T.int32]),
      expectMono 2 []
        (lambda "x" $ list [lambda "y" $ primitive _math_add @@ var "x" @@ var "y"])
        (T.function T.int32 $ T.list $ T.function T.int32 T.int32)],

    subgroup "Nested lambdas with shadowing" [
      expectPoly 1 []
        (lambda "x" $ lambda "x" $ primitive _math_add @@ var "x" @@ int32 42)
        ["t0"] (T.function (T.var "t0") (T.function T.int32 T.int32))]]

testGroupForLet :: TTerm TestGroup
testGroupForLet = supergroup "Let terms" [

    subgroup "Simple" [
      expectPoly 1  []
        (lets ["x">: float32 42.0] (lambda "y" (lambda "z" (var "x"))))
        ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") T.float32))],
    subgroup "Empty let" [
      expectMono 1 []
        (lets [] $ int32 42)
        T.int32,
      expectPoly 2 []
        (lets [] $ lambda "x" $ var "x")
        ["t0"] (T.function (T.var "t0") (T.var "t0"))],
    subgroup "Trivial let" [
      expectMono 1  []
        (lets [
          "foo">: int32 42]
          $ var "foo")
        T.int32],
    subgroup "Multiple references to a let-bound term" [
      expectMono 1 []
        (lets [
          "foo">: int32 42,
          "bar">: int32 137]
          $ list [var "foo", var "bar", var "foo"])
        (T.list T.int32)],

    subgroup "Nested let" [
      expectMono 1 []
        (lets [
          "foo">: int32 42]
          $ lets [
            "bar">: int32 137]
            $ list [var "foo", var "bar"])
        (T.list T.int32),
      expectMono 2 []
        (lets [
          "foo">: int32 42]
          $ lets [
            "bar">: tuple2 (var "foo") (int32 137)]
            $ var "bar")
        (T.tuple2 T.int32 T.int32),
      expectPoly 3 []
        (lets [
          "sng">: lambda "x" $ list [var "x"]]
          $ lets [
            "foo">: var "sng" @@ int32 42,
            "bar">: var "sng" @@ string "bar",
            "quux">: lambda "x" $ var "sng" @@ var "x"]
            $ tuple2 (var "foo") (tuple2 (var "bar") (var "quux" @@ list [])))
        ["t0"] (T.tuple2 (T.list T.int32) (T.tuple2 (T.list T.string) (T.list $ T.list $ T.var "t0")))],

    subgroup "Nested let with shadowing" [
      expectMono 1 []
        (lets [
          "foo">: string "foo"]
          $ lets [
            "foo">: int32 137]
            $ var "foo")
        T.int32,
      expectMono 2 []
        (lets [
          "foo">: string "foo",
          "bar">: var "foo"]
          $ lets [
            "foo">: int32 137]
            $ tuple2 (var "bar") (var "foo"))
        (T.tuple2 T.string T.int32)],

    subgroup "Let-polymorphism" [
      expectPoly 1 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ lambda "x" $ var "id" @@ (var "id" @@ var "x"))
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectMono 2 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ var "id" @@ (list [var "id" @@ int32 42]))
        (T.list T.int32),
      expectPoly 3 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ lambda "x" (var "id" @@ (list [var "id" @@ var "x"])))
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectMono 4 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ tuple2 (var "id" @@ int32 42) (var "id" @@ string "foo"))
        (T.tuple2 T.int32 T.string),
      expectMono 5 []
        (lets [
          "list">: lambda "x" $ list [var "x"]]
          $ tuple2 (var "list" @@ int32 42) (var "list" @@ string "foo"))
        (T.tuple2 (T.list T.int32) (T.list T.string)),
      expectPoly 6 [tag_disabled]
        (lets [
          "singleton">: lambda "x" $ list [var "x"],
          "f">: lambda "x" $ lambda "y" $ primitive _lists_cons
            @@ (tuple2 (var "singleton" @@ var "x") (var "singleton" @@ var "y"))
            @@ (var "g" @@ var "x" @@ var "y"),
          "g">: lambda "x" $ lambda "y" $ var "f" @@ int32 42 @@ var "y"]
          $ var "f")
        ["t0"] (T.list $ T.tuple2 T.int32 (T.var "t0")),
      expectMono 7 [tag_disabledForMinimalInference]
        (lets [
          "id">: lambda "x" $ var "x",
          "fortytwo">: var "id" @@ int32 42,
          "foo">: var "id" @@ string "foo"]
          $ tuple2 (var "fortytwo") (var "foo"))
        (T.tuple2 T.int32 T.string),
      expectMono 8 [tag_disabledForMinimalInference]
        (lets [
          "fortytwo">: var "id" @@ int32 42,
          "id">: lambda "x" $ var "x",
          "foo">: var "id" @@ string "foo"]
          $ tuple2 (var "fortytwo") (var "foo"))
        (T.tuple2 T.int32 T.string)],

    subgroup "Recursive and mutually recursive let (@wisnesky's test cases)" [
      expectPoly 1 []
        (lets [
          "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")]
          $ var "f")
        ["t0"] (T.function T.int32 (T.function T.int32 (T.var "t0"))),
      -- Try: :t (let (f, g) = (g, f) in (f, g))
      expectPoly 2 []
        (lets [
          "x">: var "y",
          "y">: var "x"] $
          tuple2 (var "x") (var "y"))
        ["t0", "t1"] (T.tuple2 (T.var "t0") (T.var "t1")),
      expectPoly 3 [tag_disabled]
        (lets [
          "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
          "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
          $ tuple2 (var "f") (var "g"))
        ["t0", "t1"] (T.tuple2
          (T.functionMany [T.var "t0", T.int32, T.var "t1"])
          (T.functionMany [T.int32, T.var "v0", T.var "t1"])),
      expectMono 4 []
        -- letrec + = (\x . (\y . (S (+ (P x) y)))) in (+ (S (S 0)) (S 0))
        (lets [
          "plus">: lambda "x" $ lambda "y" $ s @@ (var "plus" @@ (p @@ var "x") @@ var "y")]
          $ var "plus" @@ (s @@ (s @@ int32 0)) @@ (s @@ int32 0))
        T.int32,
      expectMono 5 []
        -- letrecs id = (\z. z)
        --     f = (\p0. (tuple2 (id p0) (id p0)))
        --     in 0
        (lets [
          "id">: lambda "z" $ var "z",
          "f">: lambda "p0" $ tuple2 (var "id" @@ var "p0") (var "id" @@ var "p0")]
          $ int32 0)
        T.int32,
      expectPoly 6 []
        (lets [
           "x">: lambda "y" $ var "y",
           "z">: var "x"] $
           tuple2 (var "x") (var "z"))
        ["t0", "t1"] (T.tuple2 (T.function (T.var "t0") (T.var "t0")) (T.function (T.var "t1") (T.var "t1"))),
      expectPoly 7 []
        (lets [
           "x">: lambda "y" $ var "y",
           "z">: var "x",
           "w">: var "z"] $
           tuple2 (var "x") (tuple2 (var "w") (var "z")))
        ["t0", "t1", "t2"] (T.product [
          T.function (T.var "t0") (T.var "t0"),
          T.product [
          T.function (T.var "t1") (T.var "t1"),
          T.function (T.var "t2") (T.var "t2")]])],

    subgroup "Recursive and mutually recursive let with polymorphism" [
      expectMono 1 []
        (lets [
          "id">: lambda "x" $ var "x",
          "f">: primitive _strings_length @@ var "g",
          "g">: primitive _strings_fromList @@ list [var "f"]]
          $ tuple2 (var "f") (var "g"))
        (T.tuple2 T.int32 T.string),
      expectMono 2 [tag_disabledForMinimalInference]
        (lets [
          "id">: lambda "x" $ var "x",
          "f">: var "id" @@ (primitive _strings_length @@ var "g"),
          "g">: var "id" @@ (primitive _strings_fromList @@ list [var "f"])]
          $ tuple2 (var "f") (var "g"))
        (T.tuple2 T.int32 T.string),
      expectMono 3 [tag_disabledForMinimalInference]
        (lets [
          "f">: var "id" @@ (primitive _strings_length @@ var "g"),
          "id">: lambda "x" $ var "x",
          "g">: var "id" @@ (primitive _strings_fromList @@ list [var "f"])]
          $ tuple2 (var "f") (var "g"))
        (T.tuple2 T.int32 T.string)],

    subgroup "Recursion involving polymorphic functions" [ -- Note: not 'polymorphic recursion' per se
      expectPoly 1 []
        (lets [
          "f">: lambda "b" $ lambda "x" $ primitive _logic_ifElse @@ var "b" @@ list [list [var "x"]] @@ (var "g" @@ var "b" @@ var "x"),
          "g">: lambda "b" $ lambda "x" $ primitive _logic_ifElse @@ var "b" @@ (var "f" @@ var "b" @@ var "x") @@ list [list [var "x"]]]
          $ var "f")
        ["t0"] (T.functionMany [T.boolean, T.var "t0", T.list $ T.list $ T.var "t0"]),

      -- The recursive pattern of hydra.rewriting.foldOverType is similar to this example.
      expectPoly 2 [tag_disabledForMinimalInference]
        (lets [
          "inst">: var "rec" @@ (lambda "x" false) @@ false,
          "rec">: lambda "f" $ lambda "b0" $ var "f" @@ (var "rec" @@ var "f" @@ var "b0")] $
          tuple2 (var "inst") (var "rec"))
        ["t0", "t1"] (T.tuple2 T.boolean (T.functionMany [T.function (T.var "t0") (T.var "t0"), T.var "t1", T.var "t0"])),
      expectPoly 3 [tag_disabledForMinimalInference] -- Try with GHC:    :t let inst = rec (\x -> False); rec = \f -> f (rec f) in (inst, rec)
        (lets [
          "inst">: var "rec" @@ (lambda "x" false),
          "rec">: lambda "f" $ var "f" @@ (var "rec" @@ var "f")] $
          tuple2 (var "inst") (var "rec"))
        ["t0"] (T.tuple2 T.boolean (T.functionMany [T.function (T.var "t0") (T.var "t0"), T.var "t0"])),
      expectPoly 4 [tag_disabledForMinimalInference]
        (lets [
          "inst1">: var "rec" @@ (lambda "x" false),
          "inst2">: var "rec" @@ (lambda "x" $ int32 42),
          "rec">: lambda "f" $ var "f" @@ (var "rec" @@ var "f")] $
          tuple [var "inst1", var "inst2", var "rec"])
        ["t0"] (T.product [T.boolean, T.int32, T.functionMany [T.function (T.var "t0") (T.var "t0"), T.var "t0"]]),

      -- Try: :t let foo = bar; bar = foo in (foo, bar)
      expectPoly 5 [tag_disabledForMinimalInference]
        (lets [
          "foo">: var "bar",
          "bar">: var "foo"] $
          tuple2 (var "foo") (var "bar"))
        ["t0", "t1"] (T.tuple2 (T.var "t0") (T.var "t1"))]]
  where
    s = primitive _math_negate
    p = primitive _math_negate

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

    subgroup "Recursion" [
      expectPoly 1 []
        (lets [
          "x">: var "x"]
          $ var "x")
        ["t0"] (T.var "t0"),
      expectPoly 2 [tag_disabledForMinimalInference]
        (lets ["id">: lambda "x" $ var "x",
               "weird">: var "id" @@ var "id" @@ var "id"] $
               var "weird")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectPoly 3 []
        (lets ["f">: lambda "x" $ var "f" @@ (var "f" @@ var "x")] $
               var "f")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectPoly 4 []
        (lets ["x">: lambda "y" $
                 var "x" @@ var "y"] $
               var "x")
        ["t0", "t1"] (T.function (T.var "t0") (T.var "t1")),
      expectPoly 5 []
        (lets ["paradox">: lambda "f" $ var "f" @@ (var "paradox" @@ var "f")] $
               var "paradox")
        ["t0"] (T.function (T.function (T.var "t0") (T.var "t0")) (T.var "t0")),
      expectMono 6 []
        (lets [
          "f">: lambda "x" $ var "g" @@ (var "f" @@ var "x"),
          "g">: lambda "y" $ var "f" @@ (var "g" @@ var "y")] $
          var "f" @@ (var "g" @@ int32 42))
        T.int32],

    subgroup "Infinite lists" [
      expectMono 1 []
        (lets [
          "self">: primitive _lists_cons @@ int32 42 @@ var "self"]
          $ var "self")
        (T.list T.int32),
      expectPoly 2  []
        (lambda "x" $ lets [
          "self">: primitive _lists_cons @@ var "x" @@ var "self"]
          $ var "self")
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectPoly 3  [tag_disabled]
        (lets [
          "self">: lambda "e" $ primitive _lists_cons @@ var "e" @@ (var "self" @@ var "e")]
          $ lambda "x" $ var "self" @@ var "x")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectMono 4  []
        (lets [
          "build">: lambda "x" $ primitive _lists_cons @@ var "x" @@ (var "build" @@
            (primitive _math_add @@ var "x" @@ int32 1))]
          $ var "build" @@ int32 0)
        (T.list T.int32)]]

testGroupForPolymorphism :: TTerm TestGroup
testGroupForPolymorphism = supergroup "Polymorphism" [

    subgroup "Simple lists and optionals" [
      expectPoly 1 []
        (list [])
        ["t0"] (T.list (T.var "t0")),
      expectPoly 2 [tag_disabledForMinimalInference]
        (optional nothing)
        ["t0"] (T.optional (T.var "t0")),
      expectMono 3 [tag_disabledForMinimalInference]
        (optional $ just $ int32 42)
        (T.optional T.int32)],

    subgroup "Lambdas, lists, and products" [
      expectPoly 1 []
        (lambda "x" $ var "x")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectPoly 2 []
        (lambda "x" $ tuple2 (var "x") (var "x"))
        ["t0"] (T.function (T.var "t0") (T.tuple2 (T.var "t0") (T.var "t0"))),
      expectPoly 3 []
        (lambda "x" $ list [var "x"])
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectPoly 4 []
        (list [lambda "x" $ var "x", lambda "y" $ var "y"])
        ["t0"] (T.list (T.function (T.var "t0") (T.var "t0"))),
      expectPoly 5 []
        (list [lambda "x" $ lambda "y" $ tuple2 (var "y") (var "x")])
        ["t0", "t1"] (T.list (T.function (T.var "t0") (T.function (T.var "t1") (T.tuple2 (T.var "t1") (T.var "t0")))))],

    subgroup "Lambdas and application" [
      expectMono 1 []
        ((lambda "x" $ var "x") @@ string "foo")
        T.string],

    subgroup "Primitives and application" [
      expectMono 1 []
        (primitive _lists_concat @@ list [list [int32 42], list []])
        (T.list T.int32)],

    subgroup "Lambdas and primitives" [
      expectMono 1 []
        (primitive _math_add)
        (T.functionMany [T.int32, T.int32, T.int32]),
      expectMono 2 []
        (lambda "x" (primitive _math_add @@ var "x"))
        (T.functionMany [T.int32, T.int32, T.int32]),
      expectMono 3 []
        (lambda "x" (primitive _math_add @@ var "x" @@ var "x"))
        (T.function T.int32 T.int32)],

    subgroup "Mixed expressions with lambdas, constants, and primitive functions" [
      expectMono 1 []
        (lambda "x" $ (primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1))
        (T.function T.int32 T.int32)]]

testGroupForPrimitives :: TTerm TestGroup
testGroupForPrimitives = supergroup "Primitives" [

    subgroup "Monomorphic primitive functions" [
      expectMono 1 []
        (primitive $ _strings_length)
        (T.function T.string T.int32),
      expectMono 2 []
        (primitive _math_sub)
        (T.functionMany [T.int32, T.int32, T.int32])],

    subgroup "Polymorphic primitive functions" [
      expectPoly 1 []
        (lambda "el" (primitive _lists_length @@ (list [var "el"])))
        ["t0"] (T.function (T.var "t0") T.int32),
      expectMono 2 []
        (lambda "el" (primitive _lists_length @@ (list [int32 42, var "el"])))
        (T.function T.int32 T.int32),
      expectPoly 3 []
        (primitive _lists_concat)
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") (T.list $ T.var "t0")),
      expectPoly 4 []
        (lambda "lists" (primitive _lists_concat @@ var "lists"))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") (T.list $ T.var "t0")),
      expectPoly 5 []
        (lambda "lists" (primitive _lists_length @@ (primitive _lists_concat @@ var "lists")))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") T.int32),
      expectPoly 6 []
        (lambda "list" (primitive _lists_length @@ (primitive _lists_concat @@ list [var "list", list []])))
        ["t0"] (T.function (T.list $ T.var "t0") T.int32),
      expectPoly 7 []
        (lambda "list" (primitive _math_add
          @@ int32 1
          @@ (primitive _lists_length @@ (primitive _lists_concat @@ list [var "list", list []]))))
        ["t0"] (T.function (T.list $ T.var "t0") T.int32),
      expectPoly 8 []
        (lambda "lists" (primitive _lists_length @@ (primitive _lists_concat @@ var "lists")))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") T.int32)]]
