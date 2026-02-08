module Hydra.Sources.Test.Inference.Fundamentals where

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


ns :: Namespace
ns = Namespace "hydra.test.inference.fundamentals"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Inference tests for fundamental language features")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding testGroupForLambdas,
      Phantoms.toBinding testGroupForLet,
      Phantoms.toBinding testGroupForLiterals,
      Phantoms.toBinding testGroupForPathologicalTerms,
      Phantoms.toBinding testGroupForPolymorphism,
      Phantoms.toBinding testGroupForPrimitives]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Fundamental language feature tests" $
  supergroup "Fundamentals" [
    testGroupForLambdas,
    testGroupForLet,
    testGroupForLiterals,
    testGroupForPathologicalTerms,
    testGroupForPolymorphism,
    testGroupForPrimitives]

testGroupForLambdas :: TBinding TestGroup
testGroupForLambdas = define "testGroupForLambdas" $
  supergroup "Lambdas" [
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

testGroupForLet :: TBinding TestGroup
testGroupForLet = define "testGroupForLet" $
  supergroup "Let terms" [

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
        T.int32,
      expectMono 2 []
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
            "bar">: pair (var "foo") (int32 137)]
            $ var "bar")
        (T.pair T.int32 T.int32),
      expectPoly 3 []
        (lets [
          "sng">: lambda "x" $ list [var "x"]]
          $ lets [
            "foo">: var "sng" @@ int32 42,
            "bar">: var "sng" @@ string "bar",
            "quux">: lambda "x" $ var "sng" @@ var "x"]
            $ pair (var "foo") (pair (var "bar") (var "quux" @@ list [])))
        ["t0"] (T.pair (T.list T.int32) (T.pair (T.list T.string) (T.list $ T.list $ T.var "t0")))],

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
            $ pair (var "bar") (var "foo"))
        (T.pair T.string T.int32)],

    subgroup "Let-polymorphism" [
      expectPoly 1 []
        (lets ["x">: float32 42.0] $ lambdas ["y", "z"] $ var "x")
        ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") T.float32)),
      -- Example from https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
      expectMono 2 []
        (lets [
            "square">: lambda "z" $ primitive _math_mul @@ var "z" @@ var "z"] $
          lambdas ["f", "x", "y"] $ primitive _logic_ifElse
              @@ (var "f" @@ (var "square" @@ var "x") @@ var "y")
              @@ (var "f" @@ var "x" @@ (var "f" @@ var "x" @@ var "y"))
              @@ (var "f" @@ var "x" @@ var "y"))
        (T.functionMany [
          T.functionMany [T.int32, T.boolean, T.boolean], T.int32, T.boolean, T.boolean]),
      expectPoly 3 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ lambda "x" $ var "id" @@ (var "id" @@ var "x"))
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectMono 4 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ var "id" @@ (list [var "id" @@ int32 42]))
        (T.list T.int32),
      expectPoly 5 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ lambda "x" (var "id" @@ (list [var "id" @@ var "x"])))
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),
      expectMono 6 []
        (lets [
          "id">: lambda "x" $ var "x"]
          $ pair (var "id" @@ int32 42) (var "id" @@ string "foo"))
        (T.pair T.int32 T.string),
      expectMono 7 []
        (lets [
          "list">: lambda "x" $ list [var "x"]]
          $ pair (var "list" @@ int32 42) (var "list" @@ string "foo"))
        (T.pair (T.list T.int32) (T.list T.string)),
      expectPoly 8 [tag_disabled]
        (lets [
          "singleton">: lambda "x" $ list [var "x"],
          "f">: lambda "x" $ lambda "y" $ primitive _lists_cons
            @@ (pair (var "singleton" @@ var "x") (var "singleton" @@ var "y"))
            @@ (var "g" @@ var "x" @@ var "y"),
          "g">: lambda "x" $ lambda "y" $ var "f" @@ int32 42 @@ var "y"]
          $ var "f")
        ["t0"] (T.list $ T.pair T.int32 (T.var "t0")),
      expectMono 9 [tag_disabledForMinimalInference]
        (lets [
          "id">: lambda "x" $ var "x",
          "fortytwo">: var "id" @@ int32 42,
          "foo">: var "id" @@ string "foo"]
          $ pair (var "fortytwo") (var "foo"))
        (T.pair T.int32 T.string),
      expectMono 10 [tag_disabledForMinimalInference]
        (lets [
          "fortytwo">: var "id" @@ int32 42,
          "id">: lambda "x" $ var "x",
          "foo">: var "id" @@ string "foo"]
          $ pair (var "fortytwo") (var "foo"))
        (T.pair T.int32 T.string)],

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
          pair (var "x") (var "y"))
        ["t0", "t1"] (T.pair (T.var "t0") (T.var "t1")),
      expectPoly 3 [tag_disabled]
        (lets [
          "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
          "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
          $ pair (var "f") (var "g"))
        ["t0", "t1"] (T.pair
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
        --     f = (\p0. (pair (id p0) (id p0)))
        --     in 0
        (lets [
          "id">: lambda "z" $ var "z",
          "f">: lambda "p0" $ pair (var "id" @@ var "p0") (var "id" @@ var "p0")]
          $ int32 0)
        T.int32,
      expectPoly 6 []
        (lets [
           "x">: lambda "y" $ var "y",
           "z">: var "x"] $
           pair (var "x") (var "z"))
        ["t0", "t1"] (T.pair (T.function (T.var "t0") (T.var "t0")) (T.function (T.var "t1") (T.var "t1"))),
      expectPoly 7 []
        (lets [
           "x">: lambda "y" $ var "y",
           "z">: var "x",
           "w">: var "z"] $
           pair (var "x") (pair (var "w") (var "z")))
        ["t0", "t1", "t2"] (T.pair
          (T.function (T.var "t0") (T.var "t0"))
          (T.pair
            (T.function (T.var "t1") (T.var "t1"))
            (T.function (T.var "t2") (T.var "t2"))))],

    subgroup "Recursive and mutually recursive let with polymorphism" [
      expectMono 1 []
        (lets [
          "id">: lambda "x" $ var "x",
          "f">: primitive _strings_length @@ var "g",
          "g">: primitive _strings_fromList @@ list [var "f"]]
          $ pair (var "f") (var "g"))
        (T.pair T.int32 T.string),
      expectMono 2 [tag_disabledForMinimalInference]
        (lets [
          "id">: lambda "x" $ var "x",
          "f">: var "id" @@ (primitive _strings_length @@ var "g"),
          "g">: var "id" @@ (primitive _strings_fromList @@ list [var "f"])]
          $ pair (var "f") (var "g"))
        (T.pair T.int32 T.string),
      expectMono 3 [tag_disabledForMinimalInference]
        (lets [
          "f">: var "id" @@ (primitive _strings_length @@ var "g"),
          "id">: lambda "x" $ var "x",
          "g">: var "id" @@ (primitive _strings_fromList @@ list [var "f"])]
          $ pair (var "f") (var "g"))
        (T.pair T.int32 T.string)],

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
          pair (var "inst") (var "rec"))
        ["t0", "t1"] (T.pair T.boolean (T.functionMany [T.function (T.var "t0") (T.var "t0"), T.var "t1", T.var "t0"])),
      expectPoly 3 [tag_disabledForMinimalInference] -- Try with GHC:    :t let inst = rec (\x -> False); rec = \f -> f (rec f) in (inst, rec)
        (lets [
          "inst">: var "rec" @@ (lambda "x" false),
          "rec">: lambda "f" $ var "f" @@ (var "rec" @@ var "f")] $
          pair (var "inst") (var "rec"))
        ["t0"] (T.pair T.boolean (T.functionMany [T.function (T.var "t0") (T.var "t0"), T.var "t0"])),
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
          pair (var "foo") (var "bar"))
        ["t0", "t1"] (T.pair (T.var "t0") (T.var "t1"))],

    -- Over-generalization of hoisted let-bindings.
    -- When a nested let-binding is hoisted to a sibling, its type may be generalized
    -- beyond what the usage context constrains.
    subgroup "Over-generalization of hoisted let-bindings" [
      -- Nested: \g. \val. let r = g val in g (fst r)
      -- g is constrained: fst(g val) is used as input to g, so g : a -> (a, b)
      -- f : forall a b. (a -> (a, b)) -> a -> (a, b)
      expectPoly 1 []
        (lambda "g" $ lambda "val" $
          lets ["r">: var "g" @@ var "val"] $
            var "g" @@ (primitive _pairs_first @@ var "r"))
        ["t0", "t1"] (T.function
          (T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t1")))
          (T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t1")))),

      -- Hoisted: let helper = \g. \val. g val; f = \g. \val. g (fst (helper g val)) in f
      -- helper generalizes to forall a b. (a -> b) -> a -> b, losing the constraint b = (a, c).
      -- So f gets: forall a b c. (a -> (b, c)) -> a -> (b, c) -- 3 vars instead of 2.
      -- This test documents the CURRENT behavior (over-generalized).
      -- TODO: if inference is improved, update to match test 1 above.
      expectPoly 2 []
        (lets [
          "helper">: lambda "g" $ lambda "val" $ var "g" @@ var "val",
          "f">: lambda "g" $ lambda "val" $
            var "g" @@ (primitive _pairs_first @@ (var "helper" @@ var "g" @@ var "val"))] $
          var "f")
        ["t0", "t1"] (T.function
          (T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t1")))
          (T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t1")))),

      -- Chain of captures: forField takes rec as a parameter, uses fst of result.
      -- forField = \rec. \val. \x. let r = rec val x in (fst r, x)
      -- Independently, forField : forall a b c d. (a -> b -> (c, d)) -> a -> b -> (c, b)
      -- This is correct: the constraint c=a only exists at the call site, not in forField itself.
      -- This pattern occurs in rewriteAndFoldTerm where hoisted bindings are more polymorphic
      -- than how they are used. The Java coder must handle this discrepancy.
      expectPoly 3 []
        (lets [
          "forField">: lambda "rec" $ lambda "val" $ lambda "x" $
            lets ["r">: var "rec" @@ var "val" @@ var "x"] $
              pair (primitive _pairs_first @@ var "r") (var "x"),
          "main">: lambda "rec" $ lambda "val" $ lambda "x" $
            var "forField" @@ var "rec" @@ var "val" @@ var "x"] $
          var "main")
        ["t0", "t1", "t2", "t3"] (T.function
          (T.functionMany [T.var "t0", T.var "t1", T.pair (T.var "t2") (T.var "t3")])
          (T.functionMany [T.var "t0", T.var "t1", T.pair (T.var "t2") (T.var "t1")])),

      -- Models the rewriteAndFoldTerm_r pattern: a case/if expression where one branch
      -- uses a helper's result and the other uses the direct value. Both branches must
      -- return the same type, which should unify the helper's accumulator type with the
      -- direct value's type.
      -- helper = \f. \val. f val               -- helper: forall a b. (a -> b) -> a -> b
      -- main = \f. \val. \b. ifElse b (helper f val) (val, 0)
      --   branch 1: helper f val => instantiates to (a -> b) -> a -> b, result b
      --   branch 2: (val, 0) => (a, Int)
      --   unify b = (a, Int), so helper f val : (a, Int)
      --   main : forall a. (a -> (a, Int)) -> a -> Bool -> (a, Int)  -- 1 var
      expectPoly 4 []
        (lets [
          "helper">: lambda "f" $ lambda "val" $ var "f" @@ var "val",
          "main">: lambda "f" $ lambda "val" $ lambda "b" $
            primitive _logic_ifElse @@ var "b"
              @@ (var "helper" @@ var "f" @@ var "val")
              @@ (pair (var "val") (int32 0))] $
          var "main")
        ["t0"] (T.functionMany [
          T.function (T.var "t0") (T.pair (T.var "t0") T.int32),
          T.var "t0",
          T.boolean,
          T.pair (T.var "t0") T.int32]),

      -- Closer to rewriteAndFoldTerm_r: forField has rec as lambda param (not sibling reference).
      -- forField = \rec. \val. let r = rec val in (fst r, snd r)
      --   rec: a -> (b, c), val: a, r: (b, c), result: (b, c) -- 3 vars
      -- main = \rec. \val. \b. ifElse b (forField rec val) (val, 0)
      --   branch 1: forField rec val => fresh instance of forField, result (b', c')
      --   branch 2: (val, 0) => (a, Int)
      --   unify (b', c') = (a, Int) => b' = a, c' = Int
      --   also a = a (from rec's first arg), so rec: a -> (a, Int)
      --   main: forall a. (a -> (a, Int)) -> a -> Bool -> (a, Int)  -- 1 var
      expectPoly 5 []
        (lets [
          "forField">: lambda "rec" $ lambda "val" $
            lets ["r">: var "rec" @@ var "val"] $
              pair (primitive _pairs_first @@ var "r") (primitive _pairs_second @@ var "r"),
          "main">: lambda "rec" $ lambda "val" $ lambda "b" $
            primitive _logic_ifElse @@ var "b"
              @@ (var "forField" @@ var "rec" @@ var "val")
              @@ (pair (var "val") (int32 0))] $
          var "main")
        ["t0"] (T.functionMany [
          T.function (T.var "t0") (T.pair (T.var "t0") T.int32),
          T.var "t0",
          T.boolean,
          T.pair (T.var "t0") T.int32]),

      -- Models the full rewriteAndFoldTerm_r pattern with an intermediate sibling "rcases".
      -- rcases = \forFields. \val. forFields val  -- rcases: forall a b. (a -> b) -> a -> b
      -- r = \forFields. \val. \b. ifElse b (rcases forFields val) (val, 0)
      --   branch 1: rcases forFields val. forFields: a -> b, val: a => result: b
      --     instantiate rcases: (a' -> b') -> a' -> b'. a' = a, b' = b. result: b
      --   branch 2: (val, 0) = (a, Int)
      --   unify b = (a, Int) => forFields: a -> (a, Int)
      --   r: forall a. (a -> (a, Int)) -> a -> Bool -> (a, Int)  -- 1 var
      expectPoly 6 []
        (lets [
          "rcases">: lambda "forFields" $ lambda "val" $ var "forFields" @@ var "val",
          "r">: lambda "forFields" $ lambda "val" $ lambda "b" $
            primitive _logic_ifElse @@ var "b"
              @@ (var "rcases" @@ var "forFields" @@ var "val")
              @@ (pair (var "val") (int32 0))] $
          var "r")
        ["t0"] (T.functionMany [
          T.function (T.var "t0") (T.pair (T.var "t0") T.int32),
          T.var "t0",
          T.boolean,
          T.pair (T.var "t0") T.int32])]]
  where
    s = primitive _math_negate
    p = primitive _math_negate

testGroupForLiterals :: TBinding TestGroup
testGroupForLiterals = define "testGroupForLiterals" $
  subgroup "Literals" [
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

testGroupForPathologicalTerms :: TBinding TestGroup
testGroupForPathologicalTerms = define "testGroupForPathologicalTerms" $
  supergroup "Pathological terms" [

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

testGroupForPolymorphism :: TBinding TestGroup
testGroupForPolymorphism = define "testGroupForPolymorphism" $
  supergroup "Polymorphism" [

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
        (T.function T.int32 T.int32)],

    subgroup "Application terms" [
      expectMono 1 []
        ((lambda "x" $ var "x") @@ string "foo")
        T.string,
      expectMono 2 []
        (lambda "x" $ primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1)
        (T.function T.int32 T.int32)]]

testGroupForPrimitives :: TBinding TestGroup
testGroupForPrimitives = define "testGroupForPrimitives" $
  supergroup "Primitives" [

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
