module Hydra.Sources.Test.Inference.Failures (failureTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Test.TestGraph

import           Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T

import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (map, sum)


failureTests :: TTerm TestGroup
failureTests = supergroup "Expected failures" [
  undefinedVariableTests,
  unificationFailureTests,
  invalidApplicationTests,
  selfApplicationTests,
  arityMismatchTests,
  recursiveTypeTests,
  occurCheckTests,
  typeConstructorMisuseTests,
  polymorphismViolationTests,
  letBindingMismatchTests,
  constraintSolverEdgeCaseTests,
  primitiveTypeErrorTests,
  complexConstraintFailureTests]

undefinedVariableTests :: TTerm TestGroup
undefinedVariableTests = supergroup "Undefined variable" [
  subgroup "Basic unbound variables" [
    expectFailure 1 []
      (var "x"),
    expectFailure 2 []
      (lambda "x" $ var "y"),
    expectFailure 3 []
      (lets ["x">: int32 42] $ var "y")],

  subgroup "Unbound in let expressions" [
    expectFailure 1 []
      (lets ["x">: var "y"] $ var "x"),
    expectFailure 2 []
      (lets ["x">: var "y", "z">: int32 42] $ var "x"),
    expectFailure 3 []
      (lets ["x">: int32 42, "y">: var "z"] $ tuple2 (var "x") (var "y"))],

  subgroup "Shadowing scope errors" [
    expectFailure 1 []
      (lambda "x" $ lets ["y">: var "x"] $ var "z"),
    expectFailure 2 []
      (lets ["x">: int32 42] $ lets ["y">: var "x"] $ var "z"),
    expectFailure 3 []
      (lets ["x">: lambda "y" $ var "z"] $ var "x")]]

unificationFailureTests :: TTerm TestGroup
unificationFailureTests = supergroup "Unification failure" [
  subgroup "Basic type mismatches" [
    expectFailure 1 []
      (primitive _math_add @@ int32 42 @@ string "foo"),
    expectFailure 2 []
      (list [int32 42, string "foo"]),
    expectFailure 3 []
      (list [list [int32 42], string "foo"]),
    expectFailure 4 []
      (tuple2 (int32 42) (string "foo") @@ string "bar")],

  subgroup "Collection type mismatches" [
    expectFailure 1 []
      (primitive _lists_cons @@ int32 42 @@ string "not a list"),
    expectFailure 2 []
      (list [int32 42, list [string "foo"]]),
    expectFailure 3 []
      (tuple2 (list [int32 42]) (list [string "foo"]) @@ int32 137),
    expectFailure 4 []
      (primitive _lists_concat @@ list [list [int32 42], list [string "foo"]])],

  subgroup "Conditional type mismatches" [
    expectFailure 1 []
      (primitive _logic_ifElse @@ true @@ int32 42 @@ string "foo"),
    expectFailure 2 []
      (primitive _logic_ifElse @@ true @@ list [int32 42] @@ string "foo"),
    expectFailure 3 []
      (primitive _logic_ifElse @@ true @@ (lambda "x" $ var "x") @@ int32 42)],

  subgroup "Polymorphic instantiation conflicts" [
    expectFailure 1 []
      (lets ["f">: lambda "x" $ var "x"] $
        list [var "f" @@ int32 42, var "f" @@ string "foo"]),
    expectFailure 2 []
      (lets ["id">: lambda "x" $ var "x"] $
        tuple2 (var "id" @@ int32 42) (var "id" @@ string "foo") @@ true),
    expectFailure 3 []
      (lets ["cons">: primitive _lists_cons] $
        list [var "cons" @@ int32 42, var "cons" @@ string "foo"])]]

invalidApplicationTests :: TTerm TestGroup
invalidApplicationTests = supergroup "Invalid application" [
  subgroup "Non-function application" [
    expectFailure 1 []
      (int32 42 @@ int32 137),
    expectFailure 2 []
      (string "foo" @@ int32 42),
    expectFailure 3 []
      (true @@ false),
    expectFailure 4 []
      (float64 3.14 @@ int32 42)],

  subgroup "Collection application" [
    expectFailure 1 []
      (list [int32 42] @@ string "bar"),
    expectFailure 2 []
      (tuple2 (int32 42) (string "foo") @@ true),
    expectFailure 3 []
      (list [] @@ int32 42),
    expectFailure 4 []
      (tuple [int32 1, int32 2, int32 3] @@ string "index")],

  subgroup "Primitive misapplication" [
    expectFailure 1 []
      (primitive _maps_empty @@ string "foo"),
    expectFailure 2 []
      (primitive _sets_empty @@ int32 42),
    expectFailure 3 []
      (optional nothing @@ string "value"),
    expectFailure 4 []
      (list [] @@ true)]]

selfApplicationTests :: TTerm TestGroup
selfApplicationTests = supergroup "Self-application" [
  subgroup "Direct self-application" [
    expectFailure 1 []
      (lambda "x" $ var "x" @@ var "x"),
    expectFailure 2 []
      (lets ["f">: var "f" @@ var "f"] $ var "f")],

  subgroup "Indirect self-application" [
    expectFailure 1 []
      (lets ["f">: lambda "x" $ var "g" @@ var "f", "g">: lambda "y" $ var "y" @@ var "y"] $ var "f"),
    expectFailure 2 []
      (lets ["a">: var "b" @@ var "a", "b">: lambda "x" $ var "x" @@ var "x"] $ var "a"),
    expectFailure 3 []
      (lets ["cycle">: lambda "f" $ var "f" @@ var "cycle"] $ var "cycle" @@ var "cycle")]]

arityMismatchTests :: TTerm TestGroup
arityMismatchTests = supergroup "Arity mismatch" [
  subgroup "Too many arguments" [
    expectFailure 1 []
      (primitive _math_add @@ int32 42 @@ int32 137 @@ int32 999),
    expectFailure 2 []
      ((lambda "x" $ lambda "y" $ var "x") @@ int32 42 @@ string "foo" @@ true),
    expectFailure 3 []
      (primitive _lists_cons @@ int32 42 @@ list [int32 137] @@ string "extra")],

  subgroup "Wrong argument types with extra args" [
    expectFailure 1 []
      (primitive _strings_length @@ int32 42 @@ string "extra"),
    expectFailure 2 []
      (primitive _logic_not @@ int32 42 @@ true),
    expectFailure 3 []
      ((lambda "x" $ int32 42) @@ string "arg" @@ int32 137 @@ true)]]

recursiveTypeTests :: TTerm TestGroup
recursiveTypeTests = supergroup "Recursive type construction" [
  subgroup "Direct recursive types" [
    expectFailure 1 []
      (lets ["x">: list [var "x"]] $ var "x"),
    expectFailure 2 []
      (lets ["x">: tuple2 (var "x") (int32 42)] $ var "x"),
    expectFailure 3 []
      (lets ["x">: tuple [var "x", var "x"]] $ var "x")],

  subgroup "Recursive function types" [
    expectFailure 1 []
      (lets ["f">: lambda "x" $ var "f"] $ var "f"),
    expectFailure 2 []
      (lets ["f">: lambda "x" $ lambda "y" $ var "f" @@ var "f"] $ var "f"),
    expectFailure 3 []
      (lets ["f">: lambda "x" $ list [var "f"]] $ var "f")],

  subgroup "Mutually recursive types" [
    expectFailure 1 []
      (lets ["x">: list [var "y"], "y">: tuple2 (var "x") (int32 42)] $ var "x"),
    expectFailure 2 []
      (lets ["a">: lambda "x" $ var "b", "b">: var "a"] $ var "a"),
    expectFailure 3 []
      (lets ["f">: list [var "g"], "g">: tuple [var "f", var "f"]] $ var "f")]]

occurCheckTests :: TTerm TestGroup
occurCheckTests = supergroup "Occur check failures" [
  subgroup "Function occur checks" [
    expectFailure 1 []
      (lets ["g">: lambda "h" $ var "g" @@ var "g" @@ var "h"] $ var "g")],

  subgroup "Mutual occur checks" [
    expectFailure 1 []
      (lets [
        "f">: lambda "x" $ var "g" @@ var "f",
        "g">: lambda "y" $ var "f" @@ var "g"] $ var "f"),
    expectFailure 2 []
      (lets [
        "a">: lambda "x" $ var "b" @@ var "a" @@ var "x",
        "b">: lambda "y" $ var "a" @@ var "b"] $ var "a"),
    expectFailure 3 []
      (lets [
        "cycle1">: var "cycle2" @@ var "cycle1",
        "cycle2">: lambda "x" $ var "cycle1" @@ var "x"] $ var "cycle1")],

  subgroup "Complex occur checks" [
    expectFailure 1 []
      (lets ["omega">: lambda "x" $ var "x" @@ var "x" @@ var "omega"] $ var "omega"),
    expectFailure 2 []
      (lets ["loop">: lambda "x" $ lambda "y" $ var "loop" @@ (var "x" @@ var "loop") @@ var "y"] $ var "loop")]]

typeConstructorMisuseTests :: TTerm TestGroup
typeConstructorMisuseTests = supergroup "Type constructor misuse" [
  subgroup "List constructor errors" [
    expectFailure 1 []
      (primitive _lists_cons @@ (list [int32 42]) @@ int32 137),  -- Wrong order
    expectFailure 2 []
      (primitive _lists_length @@ int32 42),  -- Not a list
    expectFailure 3 []
      (primitive _lists_head @@ string "not a list"),
    expectFailure 4 []
      (primitive _lists_tail @@ int32 42)],

  subgroup "String constructor errors" [
    expectFailure 1 []
      (primitive _strings_length @@ list [string "foo"]),  -- Not a string
    expectFailure 2 []
      (primitive _strings_cat @@ int32 42),
    expectFailure 3 []
      (primitive _strings_fromList @@ string "not a list"),
    expectFailure 4 []
      (primitive _strings_toList @@ int32 42)],

  subgroup "Math constructor errors" [
    expectFailure 1 []
      (primitive _math_add @@ list [int32 42] @@ int32 137),  -- Wrong type for math
    expectFailure 2 []
      (primitive _math_sub @@ string "not a number" @@ int32 42),
    expectFailure 3 []
      (primitive _math_mul @@ int32 42 @@ string "not a number"),
    expectFailure 4 []
      (primitive _math_div @@ true @@ false)]]

polymorphismViolationTests :: TTerm TestGroup
polymorphismViolationTests = supergroup "Polymorphism violations" [
  subgroup "Identity function violations" [
    expectFailure 1 []
      (lets ["id">: lambda "x" $ var "x"] $
        primitive _math_add @@ (var "id" @@ int32 42) @@ (var "id" @@ string "foo")),
    expectFailure 2 []
      (lets ["id">: lambda "x" $ var "x"] $
        list [var "id" @@ int32 42, var "id" @@ string "foo"]),
    expectFailure 3 []
      (lets ["id">: lambda "x" $ var "x"] $
        tuple2 (var "id" @@ int32 42) (var "id" @@ string "foo") @@ true)],

  subgroup "Constrained polymorphism violations" [
    expectFailure 1 []
      (lets ["f">: lambda "x" $ list [var "x", int32 42]] $ var "f" @@ string "foo"),
    expectFailure 2 []
      (lets ["g">: lambda "x" $ tuple2 (var "x") (string "constant")] $
        primitive _math_add @@ (first $ var "g" @@ int32 42) @@ (first $ var "g" @@ string "bad")),
    expectFailure 3 []
      (lets ["h">: lambda "x" $ primitive _lists_cons @@ var "x" @@ list [int32 0]] $
        var "h" @@ string "incompatible")],

  subgroup "Higher-order polymorphism violations" [
    expectFailure 1 []
      (lambda "f" $ tuple2 (var "f" @@ int32 42) (var "f" @@ string "foo")),
    expectFailure 2 []
      (lambda "g" $ list [var "g" @@ int32 1, var "g" @@ string "bad"]),
    expectFailure 3 []
      (lambda "h" $ primitive _math_add @@ (var "h" @@ int32 42) @@ (var "h" @@ string "error"))]]

letBindingMismatchTests :: TTerm TestGroup
letBindingMismatchTests = supergroup "Let binding type mismatches" [
  subgroup "Application type mismatches" [
    expectFailure 1 []
      (lets [
        "x">: int32 42,
        "y">: var "x" @@ string "foo"] $ var "y"),
    expectFailure 2 []
      (lets [
        "f">: lambda "x" $ string "result",
        "g">: var "f" @@ int32 42 @@ string "extra"] $ var "g"),
    expectFailure 3 []
      (lets [
        "num">: int32 42,
        "bad">: var "num" @@ var "num"] $ var "bad")],

  subgroup "Collection type mismatches" [
    expectFailure 1 []
      (lets [
        "list1">: list [int32 42],
        "list2">: primitive _lists_cons @@ string "foo" @@ var "list1"] $ var "list2"),
    expectFailure 2 []
      (lets [
        "nums">: list [int32 1, int32 2],
        "mixed">: primitive _lists_cons @@ string "bad" @@ var "nums"] $ var "mixed"),
    expectFailure 3 []
      (lets [
        "pair1">: tuple2 (int32 42) (string "foo"),
        "pair2">: tuple2 (string "bar") (var "pair1")] $
        primitive _math_add @@ (first $ var "pair2") @@ int32 1)],

  subgroup "Function binding mismatches" [
    expectFailure 1 []
      (lets [
        "add">: primitive _math_add,
        "badCall">: var "add" @@ string "not a number" @@ int32 42] $
        var "badCall"),
    expectFailure 2 []
      (lets [
        "f">: lambda "x" $ lambda "y" $ var "x",
        "g">: var "f" @@ int32 42,
        "bad">: var "g" @@ string "foo" @@ true] $
        var "bad")]]

constraintSolverEdgeCaseTests :: TTerm TestGroup
constraintSolverEdgeCaseTests = supergroup "Constraint solver edge cases" [
  subgroup "Complex constraint propagation" [
    expectFailure 1 []
      (lets [
        "complex">: lambda "f" $ lambda "g" $ lambda "x" $
          var "f" @@ (var "g" @@ var "x") @@ (var "g" @@ (var "f" @@ var "x")),
        "bad">: var "complex" @@ (lambda "a" $ int32 42) @@ (lambda "b" $ string "foo")] $ var "bad")],

  subgroup "Fixed point combinators" [
    expectFailure 1 []
      (lets [
        "fix">: lambda "f" $ var "f" @@ var "f",
        "bad">: var "fix" @@ (lambda "x" $ var "x" @@ var "x")] $ var "bad"),
    expectFailure 2 []
      (lets [
        "y">: lambda "f" $ (lambda "x" $ var "f" @@ (var "x" @@ var "x")) @@
                           (lambda "x" $ var "f" @@ (var "x" @@ var "x")),
        "bad">: var "y" @@ (lambda "rec" $ lambda "n" $ var "rec" @@ var "rec")] $ var "bad"),
    expectFailure 3 []
      (lets [
        "omega">: lambda "x" $ var "x" @@ var "x",
        "bad">: var "omega" @@ var "omega"] $ var "bad")],

  subgroup "Constraint cycles" [
    expectFailure 1 []
      (lets [
        "a">: lambda "x" $ var "b" @@ var "c" @@ var "x",
        "b">: lambda "y" $ var "c" @@ var "a" @@ var "y",
        "c">: lambda "z" $ var "a" @@ var "b" @@ var "z"] $
        var "a" @@ int32 42),
    expectFailure 2 []
      (lets [
        "circular">: lambda "f" $ var "f" @@ var "circular" @@ var "f"] $
        var "circular" @@ var "circular")]]

primitiveTypeErrorTests :: TTerm TestGroup
primitiveTypeErrorTests = supergroup "Primitive function type errors" [
  subgroup "Logic primitive errors" [
    expectFailure 1 []
      (primitive _logic_ifElse @@ int32 42 @@ true @@ false),  -- Condition not boolean
    expectFailure 2 []
      (primitive _logic_ifElse @@ true @@ int32 42 @@ false),  -- Branch type mismatch
    expectFailure 3 []
      (primitive _logic_and @@ int32 42 @@ true),
    expectFailure 4 []
      (primitive _logic_or @@ true @@ string "not boolean")],

  subgroup "Collection primitive errors" [
    expectFailure 1 []
      (primitive _maps_lookup @@ int32 42 @@ string "not a map"),  -- Not a map
    expectFailure 2 []
      (primitive _sets_member @@ int32 42 @@ list [int32 42]),  -- Not a set
    expectFailure 3 []
      (primitive _lists_head @@ string "not a list"),
    expectFailure 4 []
      (primitive _maybes_fromMaybe @@ int32 42 @@ string "not optional")],

  subgroup "Math primitive errors" [
    expectFailure 1 []
      (primitive _math_add @@ string "not a number" @@ int32 42),
    expectFailure 2 []
      (primitive _math_mul @@ true @@ false),
    expectFailure 3 []
      (primitive _math_div @@ list [int32 42] @@ int32 2),
    expectFailure 4 []
      (primitive _math_mod @@ int32 42 @@ string "not a number")]]

complexConstraintFailureTests :: TTerm TestGroup
complexConstraintFailureTests = supergroup "Complex constraint failures" [
  subgroup "Multi-level constraint conflicts" [
    expectFailure 1 []
      (lets [
        "f">: lambda "x" $ lambda "y" $ tuple2 (var "x") (var "y"),
        "g">: lambda "a" $ var "f" @@ var "a" @@ var "a",
        "h">: var "g" @@ (lambda "z" $ var "z")] $ var "h" @@ int32 42),
    expectFailure 2 []
      (lets [
        "weird">: lambda "f" $ lambda "x" $ var "f" @@ (var "f" @@ var "x"),
        "bad">: var "weird" @@ (lambda "y" $ tuple2 (var "y") (int32 42))] $ var "bad"),
    expectFailure 3 []
      (lets [
        "nested">: lambda "f" $ lambda "g" $ lambda "x" $
          var "f" @@ (var "g" @@ (var "f" @@ (var "g" @@ var "x"))),
        "int_f">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
        "str_g">: lambda "s" $ primitive _strings_cat @@ list [var "s", string "!"],
        "bad">: var "nested" @@ var "int_f" @@ var "str_g"] $ var "bad")],

  subgroup "Function composition failures" [
    expectFailure 1 []
      (lets [
        "triple">: lambda "f" $ lambda "x" $ var "f" @@ (var "f" @@ (var "f" @@ var "x")),
        "increment">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
        "stringify">: lambda "s" $ primitive _strings_cat @@ list [var "s", string "!"],
        "bad">: var "triple" @@ var "increment" @@ var "stringify"] $ var "bad"),
    expectFailure 2 []
      (lets [
        "compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
        "reverse_compose">: lambda "g" $ lambda "f" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
        "bad">: var "compose" @@ var "reverse_compose" @@ primitive _math_add @@ primitive _strings_length] $
        var "bad")]]
