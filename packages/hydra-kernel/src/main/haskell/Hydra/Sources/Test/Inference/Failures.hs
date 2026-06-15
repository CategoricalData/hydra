module Hydra.Sources.Test.Inference.Failures where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
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
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Logic as DefLogic
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Math as DefMath
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Pairs as DefPairs
import qualified Hydra.Lib.Sets as DefSets
import qualified Hydra.Lib.Strings as DefStrings


ns :: ModuleName
ns = ModuleName "hydra.test.inference.failures"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.inference", ModuleName "hydra.show.core"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Inference tests for expected failures"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition undefinedVariableTests,
      Phantoms.toDefinition unificationFailureTests,
      Phantoms.toDefinition invalidApplicationTests,
      Phantoms.toDefinition selfApplicationTests,
      Phantoms.toDefinition arityMismatchTests,
      Phantoms.toDefinition recursiveTypeTests,
      Phantoms.toDefinition occurCheckTests,
      Phantoms.toDefinition typeConstructorMisuseTests,
      Phantoms.toDefinition polymorphismViolationTests,
      Phantoms.toDefinition letBindingMismatchTests,
      Phantoms.toDefinition constraintSolverEdgeCaseTests,
      Phantoms.toDefinition primitiveTypeErrorTests,
      Phantoms.toDefinition complexConstraintFailureTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Expected failure tests" $
  supergroup "Expected failures" [
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

arityMismatchTests :: TypedTermDefinition TestGroup
arityMismatchTests = define "arityMismatchTests" $
  supergroup "Arity mismatch" [
  subgroup "Too many arguments" [
    expectFailure 1 []
      (primitive DefMath.add @@ int32 42 @@ int32 137 @@ int32 999),
    expectFailure 2 []
      ((lambda "x" $ lambda "y" $ var "x") @@ int32 42 @@ string "foo" @@ true),
    expectFailure 3 []
      (primitive DefLists.cons @@ int32 42 @@ list [int32 137] @@ string "extra")],

  subgroup "Wrong argument types with extra args" [
    expectFailure 1 []
      (primitive DefStrings.length @@ int32 42 @@ string "extra"),
    expectFailure 2 []
      (primitive DefLogic.not @@ int32 42 @@ true),
    expectFailure 3 []
      ((lambda "x" $ int32 42) @@ string "arg" @@ int32 137 @@ true)]]

complexConstraintFailureTests :: TypedTermDefinition TestGroup
complexConstraintFailureTests = define "complexConstraintFailureTests" $
  supergroup "Complex constraint failures" [
  subgroup "Multi-level constraint conflicts" [
    expectFailure 1 []
      (lets [
        "f">: lambda "x" $ lambda "y" $ pair (var "x") (var "y"),
        "g">: lambda "a" $ var "f" @@ var "a" @@ var "a",
        "h">: var "g" @@ (lambda "z" $ var "z")] $ var "h" @@ int32 42),
    expectFailure 2 []
      (lets [
        "weird">: lambda "f" $ lambda "x" $ var "f" @@ (var "f" @@ var "x"),
        "bad">: var "weird" @@ (lambda "y" $ pair (var "y") (int32 42))] $ var "bad"),
    expectFailure 3 []
      (lets [
        "nested">: lambda "f" $ lambda "g" $ lambda "x" $
          var "f" @@ (var "g" @@ (var "f" @@ (var "g" @@ var "x"))),
        "int_f">: lambda "n" $ primitive DefMath.add @@ var "n" @@ int32 1,
        "str_g">: lambda "s" $ primitive DefStrings.cat @@ list [var "s", string "!"],
        "bad">: var "nested" @@ var "int_f" @@ var "str_g"] $ var "bad")],

  subgroup "Function composition failures" [
    expectFailure 1 []
      (lets [
        "triple">: lambda "f" $ lambda "x" $ var "f" @@ (var "f" @@ (var "f" @@ var "x")),
        "increment">: lambda "n" $ primitive DefMath.add @@ var "n" @@ int32 1,
        "stringify">: lambda "s" $ primitive DefStrings.cat @@ list [var "s", string "!"],
        "bad">: var "triple" @@ var "increment" @@ var "stringify"] $ var "bad"),
    expectFailure 2 []
      (lets [
        "compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
        "reverse_compose">: lambda "g" $ lambda "f" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
        "bad">: var "compose" @@ var "reverse_compose" @@ primitive DefMath.add @@ primitive DefStrings.length] $
        var "bad")]]

constraintSolverEdgeCaseTests :: TypedTermDefinition TestGroup
constraintSolverEdgeCaseTests = define "constraintSolverEdgeCaseTests" $
  supergroup "Constraint solver edge cases" [
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

invalidApplicationTests :: TypedTermDefinition TestGroup
invalidApplicationTests = define "invalidApplicationTests" $
  supergroup "Invalid application" [
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
      (pair (int32 42) (string "foo") @@ true),
    expectFailure 3 []
      (list [] @@ int32 42),
    expectFailure 4 []
      (tuple [int32 1, int32 2, int32 3] @@ string "index")],

  subgroup "Primitive misapplication" [
    expectFailure 1 []
      (primitive DefMaps.empty @@ string "foo"),
    expectFailure 2 []
      (primitive DefSets.empty @@ int32 42),
    expectFailure 3 []
      (optional nothing @@ string "value"),
    expectFailure 4 []
      (list [] @@ true)]]

letBindingMismatchTests :: TypedTermDefinition TestGroup
letBindingMismatchTests = define "letBindingMismatchTests" $
  supergroup "Let binding type mismatches" [
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
        "list2">: primitive DefLists.cons @@ string "foo" @@ var "list1"] $ var "list2"),
    expectFailure 2 []
      (lets [
        "nums">: list [int32 1, int32 2],
        "mixed">: primitive DefLists.cons @@ string "bad" @@ var "nums"] $ var "mixed"),
    expectFailure 3 []
      (lets [
        "pair1">: pair (int32 42) (string "foo"),
        "pair2">: pair (string "bar") (var "pair1")] $
        primitive DefMath.add @@ (primitive DefPairs.first @@ var "pair2") @@ int32 1)],

  subgroup "Function binding mismatches" [
    expectFailure 1 []
      (lets [
        "add">: primitive DefMath.add,
        "badCall">: var "add" @@ string "not a number" @@ int32 42] $
        var "badCall"),
    expectFailure 2 []
      (lets [
        "f">: lambda "x" $ lambda "y" $ var "x",
        "g">: var "f" @@ int32 42,
        "bad">: var "g" @@ string "foo" @@ true] $
        var "bad")]]

occurCheckTests :: TypedTermDefinition TestGroup
occurCheckTests = define "occurCheckTests" $
  supergroup "Occur check failures" [
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

polymorphismViolationTests :: TypedTermDefinition TestGroup
polymorphismViolationTests = define "polymorphismViolationTests" $
  supergroup "Polymorphism violations" [
  subgroup "Identity function violations" [
    expectFailure 1 []
      (lets ["id">: lambda "x" $ var "x"] $
        primitive DefMath.add @@ (var "id" @@ int32 42) @@ (var "id" @@ string "foo")),
    expectFailure 2 []
      (lets ["id">: lambda "x" $ var "x"] $
        list [var "id" @@ int32 42, var "id" @@ string "foo"]),
    expectFailure 3 []
      (lets ["id">: lambda "x" $ var "x"] $
        pair (var "id" @@ int32 42) (var "id" @@ string "foo") @@ true)],

  subgroup "Constrained polymorphism violations" [
    expectFailure 1 []
      (lets ["f">: lambda "x" $ list [var "x", int32 42]] $ var "f" @@ string "foo"),
    expectFailure 2 []
      (lets ["g">: lambda "x" $ pair (var "x") (string "constant")] $
        primitive DefMath.add
          @@ (primitive DefPairs.first @@ (var "g" @@ int32 42))
          @@ (primitive DefPairs.first @@ (var "g" @@ string "bad"))),
    expectFailure 3 []
      (lets ["h">: lambda "x" $ primitive DefLists.cons @@ var "x" @@ list [int32 0]] $
        var "h" @@ string "incompatible")],

  subgroup "Higher-order polymorphism violations" [
    expectFailure 1 []
      (lambda "f" $ pair (var "f" @@ int32 42) (var "f" @@ string "foo")),
    expectFailure 2 []
      (lambda "g" $ list [var "g" @@ int32 1, var "g" @@ string "bad"]),
    expectFailure 3 []
      (lambda "h" $ primitive DefMath.add @@ (var "h" @@ int32 42) @@ (var "h" @@ string "error"))]]

primitiveTypeErrorTests :: TypedTermDefinition TestGroup
primitiveTypeErrorTests = define "primitiveTypeErrorTests" $
  supergroup "Primitive function type errors" [
  subgroup "Logic primitive errors" [
    expectFailure 1 []
      (primitive DefLogic.ifElse @@ int32 42 @@ true @@ false),  -- Condition not boolean
    expectFailure 2 []
      (primitive DefLogic.ifElse @@ true @@ int32 42 @@ false),  -- Branch type mismatch
    expectFailure 3 []
      (primitive DefLogic.and @@ int32 42 @@ true),
    expectFailure 4 []
      (primitive DefLogic.or @@ true @@ string "not boolean")],

  subgroup "Collection primitive errors" [
    expectFailure 1 []
      (primitive DefMaps.lookup @@ int32 42 @@ string "not a map"),  -- Not a map
    expectFailure 2 []
      (primitive DefSets.member @@ int32 42 @@ list [int32 42]),  -- Not a set
    expectFailure 3 []
      (primitive DefLists.maybeHead @@ string "not a list"),
    expectFailure 4 []
      (primitive DefOptionals.fromOptional @@ int32 42 @@ string "not optional")],

  subgroup "Math primitive errors" [
    expectFailure 1 []
      (primitive DefMath.add @@ string "not a number" @@ int32 42),
    expectFailure 2 []
      (primitive DefMath.mul @@ true @@ false),
    expectFailure 3 []
      (primitive DefMath.maybeDiv @@ list [int32 42] @@ int32 2),
    expectFailure 4 []
      (primitive DefMath.maybeMod @@ int32 42 @@ string "not a number")]]

recursiveTypeTests :: TypedTermDefinition TestGroup
recursiveTypeTests = define "recursiveTypeTests" $
  supergroup "Recursive type construction" [
  subgroup "Direct recursive types" [
    expectFailure 1 []
      (lets ["x">: list [var "x"]] $ var "x"),
    expectFailure 2 []
      (lets ["x">: pair (var "x") (int32 42)] $ var "x"),
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
      (lets ["x">: list [var "y"], "y">: pair (var "x") (int32 42)] $ var "x"),
    expectFailure 2 []
      (lets ["a">: lambda "x" $ var "b", "b">: var "a"] $ var "a"),
    expectFailure 3 []
      (lets ["f">: list [var "g"], "g">: tuple [var "f", var "f"]] $ var "f")]]

selfApplicationTests :: TypedTermDefinition TestGroup
selfApplicationTests = define "selfApplicationTests" $
  supergroup "Self-application" [
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

typeConstructorMisuseTests :: TypedTermDefinition TestGroup
typeConstructorMisuseTests = define "typeConstructorMisuseTests" $
  supergroup "Type constructor misuse" [
  subgroup "List constructor errors" [
    expectFailure 1 []
      (primitive DefLists.cons @@ (list [int32 42]) @@ int32 137),  -- Wrong order
    expectFailure 2 []
      (primitive DefLists.length @@ int32 42),  -- Not a list
    expectFailure 3 []
      (primitive DefLists.maybeHead @@ string "not a list"),
    expectFailure 4 []
      (primitive DefLists.maybeTail @@ int32 42)],

  subgroup "String constructor errors" [
    expectFailure 1 []
      (primitive DefStrings.length @@ list [string "foo"]),  -- Not a string
    expectFailure 2 []
      (primitive DefStrings.cat @@ int32 42),
    expectFailure 3 []
      (primitive DefStrings.fromList @@ string "not a list"),
    expectFailure 4 []
      (primitive DefStrings.toList @@ int32 42)],

  subgroup "Math constructor errors" [
    expectFailure 1 []
      (primitive DefMath.add @@ list [int32 42] @@ int32 137),  -- Wrong type for math
    expectFailure 2 []
      (primitive DefMath.sub @@ string "not a number" @@ int32 42),
    expectFailure 3 []
      (primitive DefMath.mul @@ int32 42 @@ string "not a number"),
    expectFailure 4 []
      (primitive DefMath.maybeDiv @@ true @@ false)]]

undefinedVariableTests :: TypedTermDefinition TestGroup
undefinedVariableTests = define "undefinedVariableTests" $
  supergroup "Undefined variable" [
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
      (lets ["x">: int32 42, "y">: var "z"] $ pair (var "x") (var "y"))],

  subgroup "Shadowing scope errors" [
    expectFailure 1 []
      (lambda "x" $ lets ["y">: var "x"] $ var "z"),
    expectFailure 2 []
      (lets ["x">: int32 42] $ lets ["y">: var "x"] $ var "z"),
    expectFailure 3 []
      (lets ["x">: lambda "y" $ var "z"] $ var "x")]]

unificationFailureTests :: TypedTermDefinition TestGroup
unificationFailureTests = define "unificationFailureTests" $
  supergroup "Unification failure" [
  subgroup "Basic type mismatches" [
    expectFailure 1 []
      (primitive DefMath.add @@ int32 42 @@ string "foo"),
    expectFailure 2 []
      (list [int32 42, string "foo"]),
    expectFailure 3 []
      (list [list [int32 42], string "foo"]),
    expectFailure 4 []
      (pair (int32 42) (string "foo") @@ string "bar")],

  subgroup "Collection type mismatches" [
    expectFailure 1 []
      (primitive DefLists.cons @@ int32 42 @@ string "not a list"),
    expectFailure 2 []
      (list [int32 42, list [string "foo"]]),
    expectFailure 3 []
      (pair (list [int32 42]) (list [string "foo"]) @@ int32 137),
    expectFailure 4 []
      (primitive DefLists.concat @@ list [list [int32 42], list [string "foo"]])],

  subgroup "Conditional type mismatches" [
    expectFailure 1 []
      (primitive DefLogic.ifElse @@ true @@ int32 42 @@ string "foo"),
    expectFailure 2 []
      (primitive DefLogic.ifElse @@ true @@ list [int32 42] @@ string "foo"),
    expectFailure 3 []
      (primitive DefLogic.ifElse @@ true @@ (lambda "x" $ var "x") @@ int32 42)],

  subgroup "Polymorphic instantiation conflicts" [
    expectFailure 1 []
      (lets ["f">: lambda "x" $ var "x"] $
        list [var "f" @@ int32 42, var "f" @@ string "foo"]),
    expectFailure 2 []
      (lets ["id">: lambda "x" $ var "x"] $
        pair (var "id" @@ int32 42) (var "id" @@ string "foo") @@ true),
    expectFailure 3 []
      (lets ["cons">: primitive DefLists.cons] $
        list [var "cons" @@ int32 42, var "cons" @@ string "foo"])]]
