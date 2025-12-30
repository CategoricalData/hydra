-- | Test cases for term reduction/evaluation mechanics
module Hydra.Sources.Test.Reduction where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Terms as T
import qualified Hydra.Dsl.Meta.Types as MetaTypes
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Dsl.Meta.Base (name)

import qualified Data.Map as M


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
      typeReductionTests,
      hoistSubtermsGroup]

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- Helper for single-binding let
letExpr :: String -> TTerm Term -> TTerm Term -> TTerm Term
letExpr varName value body = T.lets [(nm varName, value)] body

-- Helper for multi-binding let
multiLet :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
multiLet bindings body = T.lets ((\(n, v) -> (nm n, v)) <$> bindings) body

-- | Test cases for hoistSubterms
-- This function hoists subterms matching a predicate into let bindings.
-- The predicate receives the path (list of TermAccessors) and the term.
-- Hoisting only occurs within existing let expressions.
hoistSubtermsGroup :: TTerm TestGroup
hoistSubtermsGroup = subgroup "hoistSubterms" [
    -- ============================================================
    -- Test: hoistNothing predicate (identity transformation)
    -- The hoistNothing predicate never hoists anything.
    -- ============================================================

    hoistCase "hoistNothing: simple let unchanged"
      hoistPredicateNothing
      -- Input: let x = 42 in x
      (letExpr "x" (T.int32 42) (T.var "x"))
      -- Output: unchanged
      (letExpr "x" (T.int32 42) (T.var "x")),

    hoistCase "hoistNothing: let with list in body unchanged"
      hoistPredicateNothing
      -- Input: let x = 1 in [x, 2, 3]
      (letExpr "x" (T.int32 1) (T.list [T.var "x", T.int32 2, T.int32 3]))
      -- Output: unchanged - hoistNothing never hoists
      (letExpr "x" (T.int32 1) (T.list [T.var "x", T.int32 2, T.int32 3])),

    hoistCase "hoistNothing: let with application in body unchanged"
      hoistPredicateNothing
      -- Input: let f = g in f (h 42)
      (letExpr "f" (T.var "g") (T.apply (T.var "f") (T.apply (T.var "h") (T.int32 42))))
      -- Output: unchanged
      (letExpr "f" (T.var "g") (T.apply (T.var "f") (T.apply (T.var "h") (T.int32 42)))),

    -- ============================================================
    -- Test: hoistLists predicate
    -- Hoists list terms at non-top-level positions.
    -- ============================================================

    hoistCase "hoistLists: list in application argument is hoisted"
      hoistPredicateLists
      -- Input: let x = 1 in f [1, 2, 3]
      (letExpr "x" (T.int32 1) (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2, T.int32 3])))
      -- Output: the list is hoisted to a new binding _hoist_1
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.list [T.int32 1, T.int32 2, T.int32 3])]
        (T.apply (T.var "f") (T.var "_hoist_1"))),

    hoistCase "hoistLists: multiple lists are hoisted in order"
      hoistPredicateLists
      -- Input: let x = 1 in pair [1, 2] [3, 4]
      (letExpr "x" (T.int32 1)
        (T.apply (T.apply (T.var "pair") (T.list [T.int32 1, T.int32 2]))
                                         (T.list [T.int32 3, T.int32 4])))
      -- Output: both lists hoisted with sequential naming
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.list [T.int32 1, T.int32 2]),
        ("_hoist_2", T.list [T.int32 3, T.int32 4])]
        (T.apply (T.apply (T.var "pair") (T.var "_hoist_1")) (T.var "_hoist_2"))),

    hoistCase "hoistLists: list in binding value is hoisted"
      hoistPredicateLists
      -- Input: let x = f [1, 2] in x
      (letExpr "x" (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2])) (T.var "x"))
      -- Output: the list in binding is hoisted
      (multiLet [
        ("x", T.apply (T.var "f") (T.var "_hoist_1")),
        ("_hoist_1", T.list [T.int32 1, T.int32 2])]
        (T.var "x")),

    hoistCase "hoistLists: nested lists hoisted from inside out"
      hoistPredicateLists
      -- Input: let x = 1 in f [[1, 2], 3]
      (letExpr "x" (T.int32 1)
        (T.apply (T.var "f") (T.list [T.list [T.int32 1, T.int32 2], T.int32 3])))
      -- Output: inner list hoisted first, then outer list (each subterm visited once)
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.list [T.int32 1, T.int32 2]),
        ("_hoist_2", T.list [T.var "_hoist_1", T.int32 3])]
        (T.apply (T.var "f") (T.var "_hoist_2"))),

    -- ============================================================
    -- Test: hoistApplications predicate
    -- Hoists function applications at non-top-level positions.
    -- ============================================================

    hoistCase "hoistApplications: application in list element is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in [f x, y]
      (letExpr "x" (T.int32 1)
        (T.list [T.apply (T.var "f") (T.var "x"), T.var "y"]))
      -- Output: the application is hoisted
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.apply (T.var "f") (T.var "x"))]
        (T.list [T.var "_hoist_1", T.var "y"])),

    hoistCase "hoistApplications: application in record field is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in {value: f x}
      (letExpr "x" (T.int32 1)
        (T.record (nm "Data") [(nm "value", T.apply (T.var "f") (T.var "x"))]))
      -- Output: the application is hoisted
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.apply (T.var "f") (T.var "x"))]
        (T.record (nm "Data") [(nm "value", T.var "_hoist_1")])),

    hoistCase "hoistApplications: nested applications hoisted from inside out"
      hoistPredicateApplications
      -- Input: let x = 1 in [f (g x)]
      (letExpr "x" (T.int32 1)
        (T.list [T.apply (T.var "f") (T.apply (T.var "g") (T.var "x"))]))
      -- Output: inner application hoisted first, then outer
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.apply (T.var "g") (T.var "x")),
        ("_hoist_2", T.apply (T.var "f") (T.var "_hoist_1"))]
        (T.list [T.var "_hoist_2"])),

    -- ============================================================
    -- Test: hoistCaseStatements predicate
    -- Hoists case/match statements at non-top-level positions.
    -- ============================================================

    hoistCase "hoistCaseStatements: case in application argument is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = just 42 in f (match x with just y -> y | nothing -> 0)
      (letExpr "x" (T.optional $ T.just $ T.int32 42)
        (T.apply (T.var "f")
          (T.match (nm "Optional") (T.just $ T.var "x")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])))
      -- Output: the case statement is hoisted
      (multiLet [
        ("x", T.optional $ T.just $ T.int32 42),
        ("_hoist_1", T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "y" (T.var "y")),
           (nm "nothing", T.int32 0)])]
        (T.apply (T.var "f") (T.var "_hoist_1"))),

    hoistCase "hoistCaseStatements: case in list element is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = 1 in [match y with ok -> x | err -> 0]
      (letExpr "x" (T.int32 1)
        (T.list [T.match (nm "Result") (T.just $ T.var "y")
          [(nm "ok", T.var "x"),
           (nm "err", T.int32 0)]]))
      -- Output: case is hoisted
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.match (nm "Result") (T.just $ T.var "y")
          [(nm "ok", T.var "x"),
           (nm "err", T.int32 0)])]
        (T.list [T.var "_hoist_1"])),

    -- ============================================================
    -- Test: Nested let expressions
    -- Hoisting respects let boundaries - each let is processed independently.
    -- ============================================================

    hoistCase "hoistLists: nested let - inner let processed independently"
      hoistPredicateLists
      -- Input: let x = 1 in (let y = 2 in f [x, y])
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (T.apply (T.var "f") (T.list [T.var "x", T.var "y"]))))
      -- Output: the list is hoisted into the inner let
      (letExpr "x" (T.int32 1)
        (multiLet [
          ("y", T.int32 2),
          ("_hoist_1", T.list [T.var "x", T.var "y"])]
          (T.apply (T.var "f") (T.var "_hoist_1")))),

    -- ============================================================
    -- Test: Non-let terms are unchanged
    -- hoistSubtermsIntoLet only affects let expressions.
    -- ============================================================

    hoistCase "hoistLists: non-let term is unchanged"
      hoistPredicateLists
      -- Input: f [1, 2, 3] (no enclosing let)
      (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2, T.int32 3]))
      -- Output: unchanged - no let to hoist into
      (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2, T.int32 3])),

    hoistCase "hoistApplications: bare application unchanged"
      hoistPredicateApplications
      -- Input: f (g x) (no enclosing let)
      (T.apply (T.var "f") (T.apply (T.var "g") (T.var "x")))
      -- Output: unchanged
      (T.apply (T.var "f") (T.apply (T.var "g") (T.var "x"))),

    -- ============================================================
    -- Test: Lambda-bound variable capture during hoisting
    -- When hoisting a term that contains free variables which are
    -- lambda-bound between the enclosing let and the current position,
    -- those variables must be captured: the hoisted binding is wrapped
    -- in lambdas for those variables, and the reference is replaced
    -- with an application of those variables.
    -- ============================================================

    -- Case 1: Hoisted term refers to let-bound variable (no capture needed)
    hoistCase "hoistLists: term referring to let-bound variable needs no capture"
      hoistPredicateLists
      -- Input: let x = 1 in f [x, 2]
      -- The list refers to x which is let-bound, not lambda-bound
      (letExpr "x" (T.int32 1)
        (T.apply (T.var "f") (T.list [T.var "x", T.int32 2])))
      -- Output: list is hoisted without any lambda wrapping
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.list [T.var "x", T.int32 2])]
        (T.apply (T.var "f") (T.var "_hoist_1"))),

    -- Case 2: Hoisted term refers to lambda-bound variable ABOVE the let (no capture needed)
    hoistCase "hoistLists: term referring to lambda above let needs no capture"
      hoistPredicateLists
      -- Input: \y -> let x = 1 in f [y, x]
      -- y is lambda-bound above the let, so it's in parentLambdaVars
      (T.lambda "y"
        (letExpr "x" (T.int32 1)
          (T.apply (T.var "f") (T.list [T.var "y", T.var "x"]))))
      -- Output: list is hoisted without lambda wrapping (y was bound before let)
      (T.lambda "y"
        (multiLet [
          ("x", T.int32 1),
          ("_hoist_1", T.list [T.var "y", T.var "x"])]
          (T.apply (T.var "f") (T.var "_hoist_1")))),

    -- Case 3: Lambda-bound variable between let and hoisted term, but NOT free in hoisted term
    hoistCase "hoistLists: lambda-bound var not free in hoisted term needs no capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, 2])
      -- y is lambda-bound between let and list, but y does not appear in the list [x, 2]
      -- So [x, 2] should be hoisted without capturing y
      (letExpr "x" (T.int32 1)
        (T.lambda "y" (T.apply (T.var "f") (T.list [T.var "x", T.int32 2]))))
      -- Output: list [x, 2] is hoisted without lambda wrapping for y (y not free in list)
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.list [T.var "x", T.int32 2])]
        (T.lambda "y" (T.apply (T.var "f") (T.var "_hoist_1")))),

    -- Case 4: Lambda-bound variable between let and hoisted term, IS free in hoisted term
    hoistCase "hoistLists: lambda-bound var free in hoisted term requires capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, y])
      -- y is lambda-bound between let and list, and y appears in the list [x, y]
      -- So [x, y] should be hoisted with y captured
      (letExpr "x" (T.int32 1)
        (T.lambda "y" (T.apply (T.var "f") (T.list [T.var "x", T.var "y"]))))
      -- Output: _hoist_1 = \y -> [x, y], reference becomes _hoist_1 y
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.lambda "y" (T.list [T.var "x", T.var "y"]))]
        (T.lambda "y" (T.apply (T.var "f") (T.apply (T.var "_hoist_1") (T.var "y"))))),

    -- Case 5: Multiple lambda-bound variables, only some free in hoisted term
    hoistCase "hoistLists: only free lambda-bound vars are captured"
      hoistPredicateLists
      -- Input: let x = 1 in (\a -> \b -> f [x, b])
      -- Both a and b are lambda-bound between let and list
      -- But only b appears in the list [x, b], so only b is captured
      (letExpr "x" (T.int32 1)
        (T.lambda "a" (T.lambda "b" (T.apply (T.var "f") (T.list [T.var "x", T.var "b"])))))
      -- Output: _hoist_1 = \b -> [x, b], reference becomes _hoist_1 b
      (multiLet [
        ("x", T.int32 1),
        ("_hoist_1", T.lambda "b" (T.list [T.var "x", T.var "b"]))]
        (T.lambda "a" (T.lambda "b" (T.apply (T.var "f") (T.apply (T.var "_hoist_1") (T.var "b"))))))]
