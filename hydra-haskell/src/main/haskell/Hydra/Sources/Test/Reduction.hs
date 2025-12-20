-- | Test cases for term reduction/evaluation mechanics
module Hydra.Sources.Test.Reduction where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as MetaTypes
import Hydra.Dsl.Meta.Base (name)


ns :: Namespace
ns = Namespace "hydra.test.reduction"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for term reduction/evaluation mechanics"
  where
    elements = [Phantoms.toBinding allTests]

-- | Test cases for beta reduction (lambda application)
betaReductionTests :: TTerm TestGroup
betaReductionTests = subgroup "beta reduction" [
  test "identity function applied to literal"
    (lambda "x" (var "x") @@ int32 42)
    (int32 42),
  test "constant function"
    (lambda "x" (int32 1) @@ int32 42)
    (int32 1),
  test "nested application"
    (lambda "x" (lambda "y" (var "x")) @@ int32 1 @@ int32 2)
    (int32 1)]
  where
    test name input output = evalCase name input output

-- | Test cases for monomorphic primitive application
-- Property: Simple applications of a unary primitive function succeed.
-- Property: Simple applications of a binary primitive function succeed.
-- Property: Extra arguments to a primitive function are tolerated (primitive is applied, extra args remain).
monomorphicPrimitiveTests :: TTerm TestGroup
monomorphicPrimitiveTests = subgroup "monomorphic primitives" [
  -- Unary string functions
  test "toUpper on lowercase"
    (primitive _strings_toUpper @@ string "hello")
    (string "HELLO"),
  test "toUpper on mixed case"
    (primitive _strings_toUpper @@ string "Hello World")
    (string "HELLO WORLD"),
  test "toUpper on empty string"
    (primitive _strings_toUpper @@ string "")
    (string ""),
  test "toLower on uppercase"
    (primitive _strings_toLower @@ string "HELLO")
    (string "hello"),
  test "string length"
    (primitive _strings_length @@ string "hello")
    (int32 5),
  test "string length of empty"
    (primitive _strings_length @@ string "")
    (int32 0),
  -- Binary arithmetic functions
  test "add two positive integers"
    (primitive _math_add @@ int32 3 @@ int32 5)
    (int32 8),
  test "add negative and positive"
    (primitive _math_add @@ int32 (-10) @@ int32 3)
    (int32 (-7)),
  test "add with zero"
    (primitive _math_add @@ int32 0 @@ int32 42)
    (int32 42),
  test "subtract integers"
    (primitive _math_sub @@ int32 10 @@ int32 3)
    (int32 7),
  test "multiply integers"
    (primitive _math_mul @@ int32 6 @@ int32 7)
    (int32 42),
  test "multiply by zero"
    (primitive _math_mul @@ int32 100 @@ int32 0)
    (int32 0),
  test "divide integers"
    (primitive _math_div @@ int32 20 @@ int32 4)
    (int32 5),
  test "modulo"
    (primitive _math_mod @@ int32 17 @@ int32 5)
    (int32 2),
  -- Binary string functions
  test "splitOn basic"
    (primitive _strings_splitOn @@ string "," @@ string "a,b,c")
    (list [string "a", string "b", string "c"]),
  test "cat2 strings"
    (primitive _strings_cat2 @@ string "hello" @@ string "world")
    (string "helloworld")]
  -- Note: "extra arguments are tolerated" test removed; it produces non-well-typed output
  where
    test name input output = evalCase name input output

-- | Test cases for polymorphic primitive application
-- Property: Polymorphic primitives work correctly with different element types.
polymorphicPrimitiveTests :: TTerm TestGroup
polymorphicPrimitiveTests = subgroup "polymorphic primitives" [
  -- List length (polymorphic in element type)
  test "length of integer list"
    (primitive _lists_length @@ list [int32 1, int32 2, int32 3])
    (int32 3),
  test "length of string list"
    (primitive _lists_length @@ list [string "a", string "b"])
    (int32 2),
  test "length of empty list"
    (primitive _lists_length @@ list [])
    (int32 0),
  test "length of single element list"
    (primitive _lists_length @@ list [true])
    (int32 1),
  -- List head
  test "head of integer list"
    (primitive _lists_head @@ list [int32 10, int32 20, int32 30])
    (int32 10),
  test "head of string list"
    (primitive _lists_head @@ list [string "first", string "second"])
    (string "first"),
  -- List last
  test "last of integer list"
    (primitive _lists_last @@ list [int32 10, int32 20, int32 30])
    (int32 30),
  -- List concat
  test "concat two integer lists"
    (primitive _lists_concat2 @@ list [int32 1, int32 2] @@ list [int32 3, int32 4])
    (list [int32 1, int32 2, int32 3, int32 4]),
  test "concat with empty list"
    (primitive _lists_concat2 @@ list [] @@ list [int32 1, int32 2])
    (list [int32 1, int32 2]),
  -- List reverse
  test "reverse integer list"
    (primitive _lists_reverse @@ list [int32 1, int32 2, int32 3])
    (list [int32 3, int32 2, int32 1]),
  test "reverse empty list"
    (primitive _lists_reverse @@ list [])
    (list [])]
  where
    test name input output = evalCase name input output

-- | Test cases for nullary primitives (constants)
nullaryPrimitiveTests :: TTerm TestGroup
nullaryPrimitiveTests = subgroup "nullary primitives" [
  test "empty set has size zero"
    (primitive _sets_size @@ primitive _sets_empty)
    (int32 0)]
  where
    test name input output = evalCase name input output

-- | Test cases for literal values
-- Property: Literal terms are fully reduced; evaluating a literal returns the same literal.
-- Property: Literal terms cannot be applied; applying a literal to another term leaves the application unchanged.
literalValueTests :: TTerm TestGroup
literalValueTests = subgroup "literals as values" [
  -- Various literal types reduce to themselves
  test "integer literal is a value"
    (int32 42)
    (int32 42),
  test "negative integer literal"
    (int32 (-17))
    (int32 (-17)),
  test "zero integer literal"
    (int32 0)
    (int32 0),
  test "string literal is a value"
    (string "hello")
    (string "hello"),
  test "empty string literal"
    (string "")
    (string ""),
  test "string with special characters"
    (string "hello\nworld\ttab")
    (string "hello\nworld\ttab"),
  test "boolean true is a value"
    true
    true,
  test "boolean false is a value"
    false
    false,
  test "float literal is a value"
    (float64 3.14)
    (float64 3.14),
  test "negative float literal"
    (float64 (-2.718))
    (float64 (-2.718)),
  test "zero float literal"
    (float64 0.0)
    (float64 0.0)]
  -- Note: "literal applied to literal" tests removed; they produce non-well-typed output
  where
    test name input output = evalCase name input output

-- | Test cases for list reduction
listReductionTests :: TTerm TestGroup
listReductionTests = subgroup "list reduction" [
  test "empty list is a value"
    (list [])
    (list []),
  test "list of literals is a value"
    (list [int32 1, int32 2, int32 3])
    (list [int32 1, int32 2, int32 3]),
  test "list with reducible element"
    (list [lambda "x" (var "x") @@ int32 42])
    (list [int32 42])]
  where
    test name input output = evalCase name input output

-- | Test cases for optional/maybe reduction
optionalReductionTests :: TTerm TestGroup
optionalReductionTests = subgroup "optional reduction" [
  test "nothing is a value"
    (optional nothing)
    (optional nothing),
  test "just literal is a value"
    (optional $ just $ int32 42)
    (optional $ just $ int32 42),
  test "just with reducible content"
    (optional $ just $ lambda "x" (var "x") @@ int32 42)
    (optional $ just $ int32 42)]
  where
    test name input output = evalCase name input output

-- | Test cases for alpha conversion (variable renaming in lambda calculus)
-- Property: Variables are correctly substituted at all levels.
-- Property: Lambdas binding the old variable are opaque to alpha conversion (prevent variable capture).
alphaConversionTests :: TTerm TestGroup
alphaConversionTests = subgroup "alpha conversion" [
  -- Variables are substituted at the top level
  alphaCase "variable at top level"
    (var "x")
    (name "x") (name "y")
    (var "y"),
  -- Variables are substituted within subexpressions
  alphaCase "variable in list"
    (list [int32 42, var "x"])
    (name "x") (name "y")
    (list [int32 42, var "y"]),
  -- Lambdas with unrelated variables are transparent to alpha conversion
  alphaCase "lambda with different variable is transparent"
    (lambda "z" $ list [int32 42, var "x", var "z"])
    (name "x") (name "y")
    (lambda "z" $ list [int32 42, var "y", var "z"]),
  -- Lambdas of the same variable are opaque to alpha conversion (to prevent capture)
  alphaCase "lambda with same variable is opaque"
    (lambda "x" $ list [int32 42, var "x", var "z"])
    (name "x") (name "y")
    (lambda "x" $ list [int32 42, var "x", var "z"]),
  -- Nested lambdas
  alphaCase "nested lambda outer variable"
    (lambda "a" $ lambda "b" $ var "x")
    (name "x") (name "y")
    (lambda "a" $ lambda "b" $ var "y"),
  alphaCase "nested lambda shadows outer"
    (lambda "x" $ lambda "y" $ var "x")
    (name "x") (name "z")
    (lambda "x" $ lambda "y" $ var "x"),
  -- Application
  alphaCase "application with variable"
    (var "f" @@ var "x")
    (name "x") (name "y")
    (var "f" @@ var "y"),
  alphaCase "application with both variables same"
    (var "x" @@ var "x")
    (name "x") (name "y")
    (var "y" @@ var "y")]

-- | Test cases for type-level beta reduction
-- Property: Type applications of forall types are reduced by substitution.
-- Property: Non-application types are unchanged by reduction.
typeReductionTests :: TTerm TestGroup
typeReductionTests = subgroup "type reduction" [
  -- Non-application types are unchanged
  typeRedCase "unit type unchanged"
    MetaTypes.unit
    MetaTypes.unit,
  typeRedCase "string type unchanged"
    MetaTypes.string
    MetaTypes.string,
  typeRedCase "int32 type unchanged"
    MetaTypes.int32
    MetaTypes.int32,
  -- Simple type application: (forall t. t -> t) String = String -> String
  typeRedCase "identity type applied to string"
    (MetaTypes.forAll "t" (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "t")) MetaTypes.@@ MetaTypes.string)
    (MetaTypes.function MetaTypes.string MetaTypes.string),
  -- Type application with unused variable: (forall x. Int32) Bool = Int32
  typeRedCase "constant type ignores argument"
    (MetaTypes.forAll "x" MetaTypes.int32 MetaTypes.@@ MetaTypes.boolean)
    MetaTypes.int32,
  -- Nested forall application
  typeRedCase "nested forall first application"
    (MetaTypes.forAll "x" (MetaTypes.forAll "y" (MetaTypes.function (MetaTypes.var "x") (MetaTypes.var "y"))) MetaTypes.@@ MetaTypes.int32)
    (MetaTypes.forAll "y" (MetaTypes.function MetaTypes.int32 (MetaTypes.var "y"))),
  -- Full application of nested forall
  typeRedCase "nested forall both applications"
    (MetaTypes.forAll "x" (MetaTypes.forAll "y" (MetaTypes.function (MetaTypes.var "x") (MetaTypes.var "y"))) MetaTypes.@@ MetaTypes.int32 MetaTypes.@@ MetaTypes.string)
    (MetaTypes.function MetaTypes.int32 MetaTypes.string),
  -- List type application
  typeRedCase "list type applied"
    (MetaTypes.forAll "a" (MetaTypes.list (MetaTypes.var "a")) MetaTypes.@@ MetaTypes.int32)
    (MetaTypes.list MetaTypes.int32),
  -- Optional type application
  typeRedCase "optional type applied"
    (MetaTypes.forAll "a" (MetaTypes.optional (MetaTypes.var "a")) MetaTypes.@@ MetaTypes.string)
    (MetaTypes.optional MetaTypes.string)]

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for term reduction mechanics" $
    supergroup "reduction" [
      betaReductionTests,
      monomorphicPrimitiveTests,
      polymorphicPrimitiveTests,
      nullaryPrimitiveTests,
      literalValueTests,
      listReductionTests,
      optionalReductionTests,
      alphaConversionTests,
      typeReductionTests]
