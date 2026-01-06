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
      hoistSubtermsGroup,
      hoistCaseStatementsGroup]

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
-- This function hoists subterms matching a predicate into local let bindings.
-- The predicate receives the term and returns True if the term should be hoisted.
-- For each let term, immediate subterms (binding values and body) are processed:
-- matching subterms are collected and hoisted into a local let that wraps
-- that immediate subterm.
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
    -- Hoists list terms. Matching subterms within an immediate subterm
    -- are collected and wrapped in a local let around that subterm.
    -- ============================================================

    hoistCase "hoistLists: list in body is hoisted into local let"
      hoistPredicateLists
      -- Input: let x = 1 in f [1, 2, 3]
      (letExpr "x" (T.int32 1) (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2, T.int32 3])))
      -- Output: body is wrapped in local let with hoisted list
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.list [T.int32 1, T.int32 2, T.int32 3])
          (T.apply (T.var "f") (T.var "_hoist__body_1")))),

    hoistCase "hoistLists: multiple lists in body are hoisted together"
      hoistPredicateLists
      -- Input: let x = 1 in pair [1, 2] [3, 4]
      (letExpr "x" (T.int32 1)
        (T.apply (T.apply (T.var "pair") (T.list [T.int32 1, T.int32 2]))
                                         (T.list [T.int32 3, T.int32 4])))
      -- Output: body is wrapped in local let with both hoisted lists
      (letExpr "x" (T.int32 1)
        (multiLet [
          ("_hoist__body_1", T.list [T.int32 1, T.int32 2]),
          ("_hoist__body_2", T.list [T.int32 3, T.int32 4])]
          (T.apply (T.apply (T.var "pair") (T.var "_hoist__body_1")) (T.var "_hoist__body_2")))),

    hoistCase "hoistLists: list in binding value is hoisted into local let"
      hoistPredicateLists
      -- Input: let x = f [1, 2] in x
      (letExpr "x" (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2])) (T.var "x"))
      -- Output: binding value is wrapped in local let
      (letExpr "x"
        (letExpr "_hoist_x_1" (T.list [T.int32 1, T.int32 2])
          (T.apply (T.var "f") (T.var "_hoist_x_1")))
        (T.var "x")),

    hoistCase "hoistLists: nested lists hoisted from inside out"
      hoistPredicateLists
      -- Input: let x = 1 in f [[1, 2], 3]
      (letExpr "x" (T.int32 1)
        (T.apply (T.var "f") (T.list [T.list [T.int32 1, T.int32 2], T.int32 3])))
      -- Output: inner list hoisted first, then outer list
      (letExpr "x" (T.int32 1)
        (multiLet [
          ("_hoist__body_1", T.list [T.int32 1, T.int32 2]),
          ("_hoist__body_2", T.list [T.var "_hoist__body_1", T.int32 3])]
          (T.apply (T.var "f") (T.var "_hoist__body_2")))),

    -- ============================================================
    -- Test: hoistApplications predicate
    -- Hoists function applications.
    -- ============================================================

    hoistCase "hoistApplications: application in list element is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in [f x, y]
      (letExpr "x" (T.int32 1)
        (T.list [T.apply (T.var "f") (T.var "x"), T.var "y"]))
      -- Output: body is wrapped in local let
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.apply (T.var "f") (T.var "x"))
          (T.list [T.var "_hoist__body_1", T.var "y"]))),

    hoistCase "hoistApplications: application in record field is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in {value: f x}
      (letExpr "x" (T.int32 1)
        (T.record (nm "Data") [(nm "value", T.apply (T.var "f") (T.var "x"))]))
      -- Output: body is wrapped in local let
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.apply (T.var "f") (T.var "x"))
          (T.record (nm "Data") [(nm "value", T.var "_hoist__body_1")]))),

    hoistCase "hoistApplications: nested applications hoisted from inside out"
      hoistPredicateApplications
      -- Input: let x = 1 in [f (g x)]
      (letExpr "x" (T.int32 1)
        (T.list [T.apply (T.var "f") (T.apply (T.var "g") (T.var "x"))]))
      -- Output: inner application hoisted first, then outer
      (letExpr "x" (T.int32 1)
        (multiLet [
          ("_hoist__body_1", T.apply (T.var "g") (T.var "x")),
          ("_hoist__body_2", T.apply (T.var "f") (T.var "_hoist__body_1"))]
          (T.list [T.var "_hoist__body_2"]))),

    -- ============================================================
    -- Test: hoistCaseStatements predicate
    -- Hoists case/match statements.
    -- ============================================================

    hoistCase "hoistCaseStatements: case in application argument is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = just 42 in f (match x with just y -> y | nothing -> 0)
      (letExpr "x" (T.optional $ T.just $ T.int32 42)
        (T.apply (T.var "f")
          (T.match (nm "Optional") (T.just $ T.var "x")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])))
      -- Output: body is wrapped in local let with hoisted case
      (letExpr "x" (T.optional $ T.just $ T.int32 42)
        (letExpr "_hoist__body_1"
          (T.match (nm "Optional") (T.just $ T.var "x")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.apply (T.var "f") (T.var "_hoist__body_1")))),

    hoistCase "hoistCaseStatements: case in list element is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = 1 in [match y with ok -> x | err -> 0]
      (letExpr "x" (T.int32 1)
        (T.list [T.match (nm "Result") (T.just $ T.var "y")
          [(nm "ok", T.var "x"),
           (nm "err", T.int32 0)]]))
      -- Output: body is wrapped in local let
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1"
          (T.match (nm "Result") (T.just $ T.var "y")
            [(nm "ok", T.var "x"),
             (nm "err", T.int32 0)])
          (T.list [T.var "_hoist__body_1"]))),

    -- ============================================================
    -- Test: Nested let expressions
    -- Each let is processed independently; inner lets are processed first.
    -- ============================================================

    hoistCase "hoistLists: nested let - inner let processed independently"
      hoistPredicateLists
      -- Input: let x = 1 in (let y = 2 in f [x, y])
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (T.apply (T.var "f") (T.list [T.var "x", T.var "y"]))))
      -- Output: the list is hoisted in the inner let's body
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (letExpr "_hoist__body_1" (T.list [T.var "x", T.var "y"])
            (T.apply (T.var "f") (T.var "_hoist__body_1"))))),

    -- ============================================================
    -- Test: Non-let terms are unchanged
    -- hoistSubterms only processes let expressions.
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
    -- lambda-bound at an enclosing scope within the immediate subterm,
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
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.list [T.var "x", T.int32 2])
          (T.apply (T.var "f") (T.var "_hoist__body_1")))),

    -- Case 2: Hoisted term refers to lambda-bound variable ABOVE the let (no capture needed)
    hoistCase "hoistLists: term referring to lambda above let needs no capture"
      hoistPredicateLists
      -- Input: \y -> let x = 1 in f [y, x]
      -- y is lambda-bound above the let, so it's not in the immediate subterm's scope
      (T.lambda "y"
        (letExpr "x" (T.int32 1)
          (T.apply (T.var "f") (T.list [T.var "y", T.var "x"]))))
      -- Output: list is hoisted without lambda wrapping (y was bound before let)
      (T.lambda "y"
        (letExpr "x" (T.int32 1)
          (letExpr "_hoist__body_1" (T.list [T.var "y", T.var "x"])
            (T.apply (T.var "f") (T.var "_hoist__body_1"))))),

    -- Case 3: Lambda-bound variable between let and hoisted term, but NOT free in hoisted term
    hoistCase "hoistLists: lambda-bound var not free in hoisted term needs no capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, 2])
      -- y is lambda-bound between let and list, but y does not appear in the list [x, 2]
      -- So [x, 2] should be hoisted without capturing y
      (letExpr "x" (T.int32 1)
        (T.lambda "y" (T.apply (T.var "f") (T.list [T.var "x", T.int32 2]))))
      -- Output: list [x, 2] is hoisted without lambda wrapping for y (y not free in list)
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.list [T.var "x", T.int32 2])
          (T.lambda "y" (T.apply (T.var "f") (T.var "_hoist__body_1"))))),

    -- Case 4: Lambda-bound variable between let and hoisted term, IS free in hoisted term
    hoistCase "hoistLists: lambda-bound var free in hoisted term requires capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, y])
      -- y is lambda-bound between let and list, and y appears in the list [x, y]
      -- So [x, y] should be hoisted with y captured
      (letExpr "x" (T.int32 1)
        (T.lambda "y" (T.apply (T.var "f") (T.list [T.var "x", T.var "y"]))))
      -- Output: _hoist__body_1 = \y -> [x, y], reference becomes _hoist__body_1 y
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.lambda "y" (T.list [T.var "x", T.var "y"]))
          (T.lambda "y" (T.apply (T.var "f") (T.apply (T.var "_hoist__body_1") (T.var "y")))))),

    -- Case 5: Multiple lambda-bound variables, only some free in hoisted term
    hoistCase "hoistLists: only free lambda-bound vars are captured"
      hoistPredicateLists
      -- Input: let x = 1 in (\a -> \b -> f [x, b])
      -- Both a and b are lambda-bound between let and list
      -- But only b appears in the list [x, b], so only b is captured
      (letExpr "x" (T.int32 1)
        (T.lambda "a" (T.lambda "b" (T.apply (T.var "f") (T.list [T.var "x", T.var "b"])))))
      -- Output: _hoist__body_1 = \b -> [x, b], reference becomes _hoist__body_1 b
      (letExpr "x" (T.int32 1)
        (letExpr "_hoist__body_1" (T.lambda "b" (T.list [T.var "x", T.var "b"]))
          (T.lambda "a" (T.lambda "b" (T.apply (T.var "f") (T.apply (T.var "_hoist__body_1") (T.var "b"))))))),

    -- ============================================================
    -- Test: Stable naming for sibling immediate subterms
    -- Each sibling uses its parent binding name as a prefix, ensuring
    -- that changes to one sibling don't affect the names in another.
    -- ============================================================

    hoistCase "hoistLists: stable naming for binding and body"
      hoistPredicateLists
      -- Input: let x = f [1, 2] in g [3, 4]
      -- Both binding value and body have lists to hoist
      (letExpr "x" (T.apply (T.var "f") (T.list [T.int32 1, T.int32 2]))
                   (T.apply (T.var "g") (T.list [T.int32 3, T.int32 4])))
      -- Output: binding uses _hoist_x_1, body uses _hoist__body_1
      (letExpr "x"
        (letExpr "_hoist_x_1" (T.list [T.int32 1, T.int32 2])
          (T.apply (T.var "f") (T.var "_hoist_x_1")))
        (letExpr "_hoist__body_1" (T.list [T.int32 3, T.int32 4])
          (T.apply (T.var "g") (T.var "_hoist__body_1")))),

    hoistCase "hoistLists: stable naming for multiple bindings"
      hoistPredicateLists
      -- Input: let x = f [1]; y = g [2] in x
      (multiLet [
        ("x", T.apply (T.var "f") (T.list [T.int32 1])),
        ("y", T.apply (T.var "g") (T.list [T.int32 2]))]
        (T.var "x"))
      -- Output: each binding uses its own name as prefix (_hoist_x_1, _hoist_y_1)
      (multiLet [
        ("x", letExpr "_hoist_x_1" (T.list [T.int32 1])
                (T.apply (T.var "f") (T.var "_hoist_x_1"))),
        ("y", letExpr "_hoist_y_1" (T.list [T.int32 2])
                (T.apply (T.var "g") (T.var "_hoist_y_1")))]
        (T.var "x")),

    -- ============================================================
    -- Test: Polymorphic recursion - this is the key test case
    -- The new local-let approach avoids polymorphic mutual recursion
    -- by keeping hoisted bindings local to each immediate subterm.
    -- ============================================================

    hoistCase "hoistLists: polymorphic binding with self-reference below hoisted term"
      hoistPredicateLists
      -- Input: let f = \x -> pair (f x) [x, 1] in f 42
      -- f is polymorphic and has a self-reference, with a list below it
      -- With sibling hoisting, this would create: let f = ...; _hoist_f_1 = [x, 1] in ...
      -- which causes polymorphic mutual recursion issues.
      -- With local hoisting, we get: let f = (let _hoist_f_1 = ... in ...) in ...
      -- which is polymorphic nesting (OK) rather than mutual recursion.
      (letExpr "f"
        (T.lambda "x" (T.apply (T.apply (T.var "pair") (T.apply (T.var "f") (T.var "x")))
                               (T.list [T.var "x", T.int32 1])))
        (T.apply (T.var "f") (T.int32 42)))
      -- Output: the list is hoisted into a local let within f's binding value
      (letExpr "f"
        (letExpr "_hoist_f_1" (T.lambda "x" (T.list [T.var "x", T.int32 1]))
          (T.lambda "x" (T.apply (T.apply (T.var "pair") (T.apply (T.var "f") (T.var "x")))
                                 (T.apply (T.var "_hoist_f_1") (T.var "x")))))
        (T.apply (T.var "f") (T.int32 42)))]

-- | Test cases for hoistCaseStatements
-- This function hoists case statements (eliminations) that are NOT at "top level".
-- Top level means: the root, or reachable through annotations, lambda bodies,
-- or ONE application LHS. Case statements at top level can become Python match
-- statements; those not at top level need to be hoisted.
hoistCaseStatementsGroup :: TTerm TestGroup
hoistCaseStatementsGroup = subgroup "hoistCaseStatements" [
    -- ============================================================
    -- Test: Case statement at top level - should NOT be hoisted
    -- ============================================================

    hoistCaseStatementsCase "case at top level of let body is NOT hoisted"
      -- Input: let x = just 42 in match x with just y -> y | nothing -> 0
      -- The case statement is directly in the let body (top level)
      (letExpr "x" (T.optional $ T.just $ T.int32 42)
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "y" (T.var "y")),
           (nm "nothing", T.int32 0)]))
      -- Output: unchanged - case is at top level
      (letExpr "x" (T.optional $ T.just $ T.int32 42)
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "y" (T.var "y")),
           (nm "nothing", T.int32 0)])),

    hoistCaseStatementsCase "case in let binding value is NOT hoisted"
      -- Input: let x = match y with just z -> z | nothing -> 0 in x
      -- The case statement is at top level of binding value
      (letExpr "x"
        (T.match (nm "Optional") (T.just $ T.var "y")
          [(nm "just", T.lambda "z" (T.var "z")),
           (nm "nothing", T.int32 0)])
        (T.var "x"))
      -- Output: unchanged - case is at top level
      (letExpr "x"
        (T.match (nm "Optional") (T.just $ T.var "y")
          [(nm "just", T.lambda "z" (T.var "z")),
           (nm "nothing", T.int32 0)])
        (T.var "x")),

    hoistCaseStatementsCase "case inside lambda body is NOT hoisted"
      -- Input: let f = \a -> match a with just y -> y | nothing -> 0 in f (just 42)
      -- The case is inside a lambda body, but lambda bodies are pass-through
      -- This becomes def f(a): match a: ... in Python
      (letExpr "f"
        (T.lambda "a"
          (T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.apply (T.var "f") (T.optional $ T.just $ T.int32 42)))
      -- Output: unchanged - case is at top level (through lambda body)
      (letExpr "f"
        (T.lambda "a"
          (T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.apply (T.var "f") (T.optional $ T.just $ T.int32 42))),

    hoistCaseStatementsCase "case inside nested lambdas is NOT hoisted"
      -- Input: let f = \a -> \b -> match a with ok -> b | err -> 0 in f
      -- The case is inside nested lambdas - still at top level
      -- This becomes def f(a, b): match a: ... in Python
      (letExpr "f"
        (T.lambda "a" (T.lambda "b"
          (T.match (nm "Result") (T.just $ T.var "a")
            [(nm "ok", T.var "b"),
             (nm "err", T.int32 0)])))
        (T.var "f"))
      -- Output: unchanged - case is at top level (through lambda bodies)
      (letExpr "f"
        (T.lambda "a" (T.lambda "b"
          (T.match (nm "Result") (T.just $ T.var "a")
            [(nm "ok", T.var "b"),
             (nm "err", T.int32 0)])))
        (T.var "f")),

    hoistCaseStatementsCase "case as LHS of one application is NOT hoisted"
      -- Input: let f = (match Optional with ...) x in f
      -- The case is LHS of one application - still at top level (one app LHS allowed)
      -- This is match taking its single argument
      (letExpr "f"
        (T.apply
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.var "x"))
        (T.var "f"))
      -- Output: unchanged - case is at top level (one application LHS)
      (letExpr "f"
        (T.apply
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.var "x"))
        (T.var "f")),

    hoistCaseStatementsCase "case wrapped in annotation is NOT hoisted"
      -- Input: let f = @ann (match Optional with ...) in f
      -- The case is wrapped in annotation - annotations are transparent
      (letExpr "f"
        (T.annot emptyAnnMap
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.var "f"))
      -- Output: unchanged - case is at top level (through annotation)
      (letExpr "f"
        (T.annot emptyAnnMap
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.var "f")),

    hoistCaseStatementsCase "case in lambda with one application is NOT hoisted"
      -- Input: let f = \a -> (match Optional with ...) a in f
      -- Lambda body + one application LHS = still at top level
      (letExpr "f"
        (T.lambda "a"
          (T.apply
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])
            (T.var "a")))
        (T.var "f"))
      -- Output: unchanged - case is at top level
      (letExpr "f"
        (T.lambda "a"
          (T.apply
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])
            (T.var "a")))
        (T.var "f")),

    -- ============================================================
    -- Test: Case statement NOT at top level - SHOULD be hoisted
    -- ============================================================

    hoistCaseStatementsCase "case as RHS of application IS hoisted"
      -- Input: let f = g (match Optional with ...) in f
      -- The case is RHS of application (argument position) - NOT top level
      (letExpr "f"
        (T.apply (T.var "g")
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.apply (T.var "g") (T.var "_hoist_f_1")))
        (T.var "f")),

    hoistCaseStatementsCase "case in nested application LHS IS hoisted"
      -- Input: let f = ((match Optional with ...) x) y in f
      -- The case is LHS of LHS of application - only ONE app LHS allowed
      -- The second application takes us out of top level
      (letExpr "f"
        (T.apply
          (T.apply
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "z" (T.lambda "w" (T.var "z"))),
               (nm "nothing", T.lambda "w" (T.int32 0))])
            (T.var "x"))
          (T.var "y"))
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "z" (T.lambda "w" (T.var "z"))),
             (nm "nothing", T.lambda "w" (T.int32 0))])
          (T.apply (T.apply (T.var "_hoist_f_1") (T.var "x")) (T.var "y")))
        (T.var "f")),

    hoistCaseStatementsCase "case inside list element IS hoisted"
      -- Input: let f = [match Optional with ...] in f
      -- The case is inside a list element - NOT top level
      (letExpr "f"
        (T.list [T.match (nm "Optional") T.nothing
          [(nm "just", T.lambda "y" (T.var "y")),
           (nm "nothing", T.int32 0)]])
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.list [T.var "_hoist_f_1"]))
        (T.var "f")),

    hoistCaseStatementsCase "case inside lambda inside list IS hoisted"
      -- Input: let f = [\a -> match a with ...] in f
      -- Even though case is inside lambda, the lambda itself is inside a list
      -- The list position makes it not top level
      (letExpr "f"
        (T.list [T.lambda "a"
          (T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])])
        (T.var "f"))
      -- Output: case is hoisted with 'a' captured
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.lambda "a"
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)]))
          (T.list [T.lambda "a" (T.apply (T.var "_hoist_f_1") (T.var "a"))]))
        (T.var "f")),

    -- ============================================================
    -- Test: Non-case terms - should NOT be hoisted regardless
    -- ============================================================

    hoistCaseStatementsCase "list inside lambda is NOT hoisted (only case statements)"
      -- Input: let f = \a -> [a, 1, 2] in f 0
      -- The list is not at top level, but hoistCaseStatements only hoists cases
      (letExpr "f"
        (T.lambda "a" (T.list [T.var "a", T.int32 1, T.int32 2]))
        (T.apply (T.var "f") (T.int32 0)))
      -- Output: unchanged - only case statements are hoisted
      (letExpr "f"
        (T.lambda "a" (T.list [T.var "a", T.int32 1, T.int32 2]))
        (T.apply (T.var "f") (T.int32 0))),

    -- ============================================================
    -- Test: Mixed scenarios
    -- ============================================================

    hoistCaseStatementsCase "case in binding is not hoisted, case in arg position is hoisted"
      -- Input: let x = match a with ... in f (match b with ...)
      -- First case is at top level of binding, second is in argument position
      (letExpr "x"
        (T.match (nm "Optional") (T.just $ T.var "a")
          [(nm "just", T.lambda "z" (T.var "z")),
           (nm "nothing", T.int32 0)])
        (T.apply (T.var "f")
          (T.match (nm "Optional") (T.just $ T.var "b")
            [(nm "just", T.lambda "w" (T.var "w")),
             (nm "nothing", T.int32 0)])))
      -- Output: only second case is hoisted
      (letExpr "x"
        (T.match (nm "Optional") (T.just $ T.var "a")
          [(nm "just", T.lambda "z" (T.var "z")),
           (nm "nothing", T.int32 0)])
        (letExpr "_hoist__body_1"
          (T.match (nm "Optional") (T.just $ T.var "b")
            [(nm "just", T.lambda "w" (T.var "w")),
             (nm "nothing", T.int32 0)])
          (T.apply (T.var "f") (T.var "_hoist__body_1")))),

    -- ============================================================
    -- Test: Mixed let and lambda at top level (no hoisting needed)
    -- ============================================================

    hoistCaseStatementsCase "case in nested let body is NOT hoisted"
      -- Input: let x = 1 in let y = 2 in match z with ...
      -- The case is in nested let body - still at top level
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (T.match (nm "Optional") (T.just $ T.var "z")
            [(nm "just", T.lambda "w" (T.var "w")),
             (nm "nothing", T.int32 0)])))
      -- Output: unchanged
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (T.match (nm "Optional") (T.just $ T.var "z")
            [(nm "just", T.lambda "w" (T.var "w")),
             (nm "nothing", T.int32 0)]))),

    hoistCaseStatementsCase "case in let inside lambda is NOT hoisted"
      -- Input: let f = \a -> let x = 1 in match a with ...
      -- Lambda body then let body - both pass through
      (letExpr "f"
        (T.lambda "a"
          (letExpr "x" (T.int32 1)
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])))
        (T.var "f"))
      -- Output: unchanged
      (letExpr "f"
        (T.lambda "a"
          (letExpr "x" (T.int32 1)
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])))
        (T.var "f")),

    hoistCaseStatementsCase "case in lambda inside let body is NOT hoisted"
      -- Input: let x = 1 in \a -> match a with ...
      -- Let body then lambda body - both pass through
      (letExpr "x" (T.int32 1)
        (T.lambda "a"
          (T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])))
      -- Output: unchanged
      (letExpr "x" (T.int32 1)
        (T.lambda "a"
          (T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))),

    hoistCaseStatementsCase "case with let+lambda+app is NOT hoisted"
      -- Input: let f = \a -> let x = 1 in (match a with ...) x
      -- Lambda body, let body, one app LHS - all pass through
      (letExpr "f"
        (T.lambda "a"
          (letExpr "x" (T.int32 1)
            (T.apply
              (T.match (nm "Optional") T.nothing
                [(nm "just", T.lambda "y" (T.var "y")),
                 (nm "nothing", T.int32 0)])
              (T.var "x"))))
        (T.var "f"))
      -- Output: unchanged
      (letExpr "f"
        (T.lambda "a"
          (letExpr "x" (T.int32 1)
            (T.apply
              (T.match (nm "Optional") T.nothing
                [(nm "just", T.lambda "y" (T.var "y")),
                 (nm "nothing", T.int32 0)])
              (T.var "x"))))
        (T.var "f")),

    -- ============================================================
    -- Test: Multiple applications (hoisting required)
    -- ============================================================

    hoistCaseStatementsCase "case in triple application LHS IS hoisted"
      -- Input: let f = (((match ...) x) y) z in f
      -- Three nested applications - only one app LHS allowed
      (letExpr "f"
        (T.apply
          (T.apply
            (T.apply
              (T.match (nm "Optional") T.nothing
                [(nm "just", T.lambda "a" (T.lambda "b" (T.lambda "c" (T.var "a")))),
                 (nm "nothing", T.lambda "b" (T.lambda "c" (T.int32 0)))])
              (T.var "x"))
            (T.var "y"))
          (T.var "z"))
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "a" (T.lambda "b" (T.lambda "c" (T.var "a")))),
             (nm "nothing", T.lambda "b" (T.lambda "c" (T.int32 0)))])
          (T.apply (T.apply (T.apply (T.var "_hoist_f_1") (T.var "x")) (T.var "y")) (T.var "z")))
        (T.var "f")),

    hoistCaseStatementsCase "case as second argument IS hoisted"
      -- Input: let f = g x (match ...) in f
      -- Case is RHS of second application
      (letExpr "f"
        (T.apply (T.apply (T.var "g") (T.var "x"))
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.apply (T.apply (T.var "g") (T.var "x")) (T.var "_hoist_f_1")))
        (T.var "f")),

    hoistCaseStatementsCase "case in both arguments - both hoisted"
      -- Input: let f = g (match a ...) (match b ...) in f
      -- Both cases are in argument positions
      (letExpr "f"
        (T.apply
          (T.apply (T.var "g")
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "x" (T.var "x")),
               (nm "nothing", T.int32 0)]))
          (T.match (nm "Optional") (T.just $ T.var "b")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 1)]))
        (T.var "f"))
      -- Output: both cases hoisted into a SINGLE let with two bindings
      -- (hoistSubterms collects all hoistable terms from one subterm into one let)
      (letExpr "f"
        (T.lets
          [(nm "_hoist_f_1",
            T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "x" (T.var "x")),
               (nm "nothing", T.int32 0)]),
           (nm "_hoist_f_2",
            T.match (nm "Optional") (T.just $ T.var "b")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 1)])]
          (T.apply (T.apply (T.var "g") (T.var "_hoist_f_1")) (T.var "_hoist_f_2")))
        (T.var "f")),

    -- ============================================================
    -- Test: Descent into various structures (hoisting required)
    -- ============================================================

    hoistCaseStatementsCase "case in second list element IS hoisted"
      -- Input: let f = [1, match ...] in f
      (letExpr "f"
        (T.list [T.int32 1,
          T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]])
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.list [T.int32 1, T.var "_hoist_f_1"]))
        (T.var "f")),

    hoistCaseStatementsCase "multiple cases in list - all hoisted"
      -- Input: let f = [match a ..., match b ...] in f
      (letExpr "f"
        (T.list [
          T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "x" (T.var "x")),
             (nm "nothing", T.int32 0)],
          T.match (nm "Optional") (T.just $ T.var "b")
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 1)]])
        (T.var "f"))
      -- Output: both cases hoisted into a SINGLE let with two bindings
      (letExpr "f"
        (T.lets
          [(nm "_hoist_f_1",
            T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "x" (T.var "x")),
               (nm "nothing", T.int32 0)]),
           (nm "_hoist_f_2",
            T.match (nm "Optional") (T.just $ T.var "b")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 1)])]
          (T.list [T.var "_hoist_f_1", T.var "_hoist_f_2"]))
        (T.var "f")),

    hoistCaseStatementsCase "case in pair first element IS hoisted"
      -- Input: let f = (match ..., 1) in f
      (letExpr "f"
        (T.pair
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.int32 1))
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.pair (T.var "_hoist_f_1") (T.int32 1)))
        (T.var "f")),

    hoistCaseStatementsCase "case in pair second element IS hoisted"
      -- Input: let f = (1, match ...) in f
      (letExpr "f"
        (T.pair
          (T.int32 1)
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)]))
        (T.var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.pair (T.int32 1) (T.var "_hoist_f_1")))
        (T.var "f")),

    -- ============================================================
    -- Test: Nested let terms - case hoisted into CHILD let, not parent
    -- This is critical: hoisting is local to immediate subterms
    -- ============================================================

    hoistCaseStatementsCase "case in child let binding hoisted into child"
      -- Input: let outer = (let inner = g (match ...) in inner) in outer
      -- The case is in the binding of inner let, inside argument position
      -- It should be hoisted into the inner let, not the outer
      (letExpr "outer"
        (letExpr "inner"
          (T.apply (T.var "g")
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)]))
          (T.var "inner"))
        (T.var "outer"))
      -- Output: case hoisted into inner let's binding
      (letExpr "outer"
        (letExpr "inner"
          (letExpr "_hoist_inner_1"
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])
            (T.apply (T.var "g") (T.var "_hoist_inner_1")))
          (T.var "inner"))
        (T.var "outer")),

    hoistCaseStatementsCase "case in child let body hoisted into child"
      -- Input: let outer = (let inner = 1 in g (match ...)) in outer
      -- The case is in the body of inner let, inside argument position
      -- It should be hoisted into the inner let's body, not the outer
      (letExpr "outer"
        (letExpr "inner" (T.int32 1)
          (T.apply (T.var "g")
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])))
        (T.var "outer"))
      -- Output: case hoisted into inner let's body
      (letExpr "outer"
        (letExpr "inner" (T.int32 1)
          (letExpr "_hoist__body_1"
            (T.match (nm "Optional") T.nothing
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)])
            (T.apply (T.var "g") (T.var "_hoist__body_1"))))
        (T.var "outer")),

    hoistCaseStatementsCase "case at top level of child let NOT hoisted"
      -- Input: let outer = (let inner = match ... in inner) in outer
      -- The case is at top level of inner let's binding - no hoisting needed
      (letExpr "outer"
        (letExpr "inner"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.var "inner"))
        (T.var "outer"))
      -- Output: unchanged
      (letExpr "outer"
        (letExpr "inner"
          (T.match (nm "Optional") T.nothing
            [(nm "just", T.lambda "y" (T.var "y")),
             (nm "nothing", T.int32 0)])
          (T.var "inner"))
        (T.var "outer")),

    hoistCaseStatementsCase "cases in both outer and child - each hoisted locally"
      -- Input: let outer = f (match a ...) (let inner = g (match b ...) in inner) in outer
      -- First case in outer's body (arg position), second in inner's binding (arg position)
      -- Each should be hoisted into its respective scope
      -- Each binding uses its name as prefix: inner gets _hoist_inner_1, outer gets _hoist_outer_1
      (letExpr "outer"
        (T.apply
          (T.apply (T.var "f")
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "x" (T.var "x")),
               (nm "nothing", T.int32 0)]))
          (letExpr "inner"
            (T.apply (T.var "g")
              (T.match (nm "Optional") (T.just $ T.var "b")
                [(nm "just", T.lambda "y" (T.var "y")),
                 (nm "nothing", T.int32 1)]))
            (T.var "inner")))
        (T.var "outer"))
      -- Output: outer binding gets _hoist_outer_1, inner binding gets _hoist_inner_1
      (letExpr "outer"
        (letExpr "_hoist_outer_1"
          (T.match (nm "Optional") (T.just $ T.var "a")
            [(nm "just", T.lambda "x" (T.var "x")),
             (nm "nothing", T.int32 0)])
          (T.apply
            (T.apply (T.var "f") (T.var "_hoist_outer_1"))
            (letExpr "inner"
              (letExpr "_hoist_inner_1"
                (T.match (nm "Optional") (T.just $ T.var "b")
                  [(nm "just", T.lambda "y" (T.var "y")),
                   (nm "nothing", T.int32 1)])
                (T.apply (T.var "g") (T.var "_hoist_inner_1")))
              (T.var "inner"))))
        (T.var "outer")),

    -- ============================================================
    -- Test: Lambda after app LHS - should trigger hoisting
    -- ============================================================

    hoistCaseStatementsCase "lambda after app LHS takes us out of top level"
      -- Input: let f = ((\a -> match a with ...) x) in f
      -- App LHS then lambda body - after using app, lambda doesn't help
      (letExpr "f"
        (T.apply
          (T.lambda "a"
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)]))
          (T.var "x"))
        (T.var "f"))
      -- Output: case is hoisted because lambda comes after we've used our one app LHS
      -- Path to case: letBinding, applicationFunction, lambdaBody
      -- Processing: letBinding (pass), applicationFunction (use app, mark usedApp=true), lambdaBody (usedApp=true, fail)
      -- So case IS hoisted. The case uses 'a' which is lambda-bound, so it's wrapped in a lambda
      -- and the reference becomes (_hoist_f_1 a)
      (letExpr "f"
        (letExpr "_hoist_f_1"
          -- The hoisted case is wrapped in a lambda to capture 'a'
          (T.lambda "a"
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "y" (T.var "y")),
               (nm "nothing", T.int32 0)]))
          -- The original lambda body is replaced with (_hoist_f_1 a)
          (T.apply
            (T.lambda "a" (T.apply (T.var "_hoist_f_1") (T.var "a")))
            (T.var "x")))
        (T.var "f")),

    -- ============================================================
    -- Test: Case statements inside case branches (nested cases)
    -- Case branches bind variables, so they are like lambda bodies
    -- ============================================================

    hoistCaseStatementsCase "case inside case branch is NOT hoisted"
      -- Input: let f = match x with just a -> match a with ... | nothing -> 0 in f
      -- Inner case is inside a case branch - branches are pass-through like lambdas
      (letExpr "f"
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "a"
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "b" (T.var "b")),
               (nm "nothing", T.int32 0)])),
           (nm "nothing", T.int32 0)])
        (T.var "f"))
      -- Output: unchanged - inner case is at top level (through case branch)
      (letExpr "f"
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "a"
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "b" (T.var "b")),
               (nm "nothing", T.int32 0)])),
           (nm "nothing", T.int32 0)])
        (T.var "f")),

    hoistCaseStatementsCase "case inside case default branch is NOT hoisted"
      -- Input: let f = match x with just a -> a | nothing -> match y with ... in f
      -- Inner case is in default branch - still pass-through
      (letExpr "f"
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "a" (T.var "a")),
           (nm "nothing",
            T.match (nm "Optional") (T.just $ T.var "y")
              [(nm "just", T.lambda "b" (T.var "b")),
               (nm "nothing", T.int32 0)])])
        (T.var "f"))
      -- Output: unchanged - inner case is at top level (through case default)
      (letExpr "f"
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "a" (T.var "a")),
           (nm "nothing",
            T.match (nm "Optional") (T.just $ T.var "y")
              [(nm "just", T.lambda "b" (T.var "b")),
               (nm "nothing", T.int32 0)])])
        (T.var "f")),

    hoistCaseStatementsCase "case in arg position inside case branch IS hoisted"
      -- Input: let f = match x with just a -> g (match a with ...) | nothing -> 0 in f
      -- Inner case is in argument position inside a case branch - should be hoisted
      (letExpr "f"
        (T.match (nm "Optional") (T.just $ T.var "x")
          [(nm "just", T.lambda "a"
            (T.apply (T.var "g")
              (T.match (nm "Optional") (T.just $ T.var "a")
                [(nm "just", T.lambda "b" (T.var "b")),
                 (nm "nothing", T.int32 0)]))),
           (nm "nothing", T.int32 0)])
        (T.var "f"))
      -- Output: inner case is hoisted to the OUTER let level (not inside the branch)
      -- because hoistSubterms only creates lets at existing let boundaries.
      -- The inner case uses 'a' which is lambda-bound, so it's wrapped in a lambda
      -- and the reference becomes (_hoist_f_1 a)
      (letExpr "f"
        (letExpr "_hoist_f_1"
          -- The inner case wrapped in a lambda to capture 'a'
          (T.lambda "a"
            (T.match (nm "Optional") (T.just $ T.var "a")
              [(nm "just", T.lambda "b" (T.var "b")),
               (nm "nothing", T.int32 0)]))
          -- The outer case with the reference (_hoist_f_1 a) inside the branch
          (T.match (nm "Optional") (T.just $ T.var "x")
            [(nm "just", T.lambda "a"
              (T.apply (T.var "g") (T.apply (T.var "_hoist_f_1") (T.var "a")))),
             (nm "nothing", T.int32 0)]))
        (T.var "f"))]

