-- | Test cases for hoisting transformations
module Hydra.Sources.Test.Hoisting where

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

import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.hoisting"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for hoisting transformations"
  where
    elements = [Phantoms.toBinding allTests]

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hoisting transformations" $
    supergroup "hoisting" [
      hoistSubtermsGroup,
      hoistCaseStatementsGroup,
      hoistLetBindingsGroup,
      hoistPolymorphicLetBindingsGroup,
      hoistPolymorphicTypeParametersGroup]

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- Helper for single-binding let
letExpr :: String -> TTerm Term -> TTerm Term -> TTerm Term
letExpr varName value body = lets [(nm varName, value)] body

-- Helper for multi-binding let
multiLet :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
multiLet bindings body = lets ((\(n, v) -> (nm n, v)) <$> bindings) body

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
      (letExpr "x" (int32 42) (var "x"))
      -- Output: unchanged
      (letExpr "x" (int32 42) (var "x")),

    hoistCase "hoistNothing: let with list in body unchanged"
      hoistPredicateNothing
      -- Input: let x = 1 in [x, 2, 3]
      (letExpr "x" (int32 1) (list [var "x", int32 2, int32 3]))
      -- Output: unchanged - hoistNothing never hoists
      (letExpr "x" (int32 1) (list [var "x", int32 2, int32 3])),

    hoistCase "hoistNothing: let with application in body unchanged"
      hoistPredicateNothing
      -- Input: let f = g in f (h 42)
      (letExpr "f" (var "g") (apply (var "f") (apply (var "h") (int32 42))))
      -- Output: unchanged
      (letExpr "f" (var "g") (apply (var "f") (apply (var "h") (int32 42)))),

    -- ============================================================
    -- Test: hoistLists predicate
    -- Hoists list terms. Matching subterms within an immediate subterm
    -- are collected and wrapped in a local let around that subterm.
    -- ============================================================

    hoistCase "hoistLists: list in body is hoisted into local let"
      hoistPredicateLists
      -- Input: let x = 1 in f [1, 2, 3]
      (letExpr "x" (int32 1) (apply (var "f") (list [int32 1, int32 2, int32 3])))
      -- Output: body is wrapped in local let with hoisted list
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (list [int32 1, int32 2, int32 3])
          (apply (var "f") (var "_hoist__body_1")))),

    hoistCase "hoistLists: multiple lists in body are hoisted together"
      hoistPredicateLists
      -- Input: let x = 1 in pair [1, 2] [3, 4]
      (letExpr "x" (int32 1)
        (apply (apply (var "pair") (list [int32 1, int32 2]))
                                         (list [int32 3, int32 4])))
      -- Output: body is wrapped in local let with both hoisted lists
      (letExpr "x" (int32 1)
        (multiLet [
          ("_hoist__body_1", list [int32 1, int32 2]),
          ("_hoist__body_2", list [int32 3, int32 4])]
          (apply (apply (var "pair") (var "_hoist__body_1")) (var "_hoist__body_2")))),

    hoistCase "hoistLists: list in binding value is hoisted into local let"
      hoistPredicateLists
      -- Input: let x = f [1, 2] in x
      (letExpr "x" (apply (var "f") (list [int32 1, int32 2])) (var "x"))
      -- Output: binding value is wrapped in local let
      (letExpr "x"
        (letExpr "_hoist_x_1" (list [int32 1, int32 2])
          (apply (var "f") (var "_hoist_x_1")))
        (var "x")),

    hoistCase "hoistLists: nested lists hoisted from inside out"
      hoistPredicateLists
      -- Input: let x = 1 in f [[1, 2], 3]
      (letExpr "x" (int32 1)
        (apply (var "f") (list [list [int32 1, int32 2], int32 3])))
      -- Output: inner list hoisted first, then outer list
      (letExpr "x" (int32 1)
        (multiLet [
          ("_hoist__body_1", list [int32 1, int32 2]),
          ("_hoist__body_2", list [var "_hoist__body_1", int32 3])]
          (apply (var "f") (var "_hoist__body_2")))),

    -- ============================================================
    -- Test: hoistApplications predicate
    -- Hoists function applications.
    -- ============================================================

    hoistCase "hoistApplications: application in list element is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in [f x, y]
      (letExpr "x" (int32 1)
        (list [apply (var "f") (var "x"), var "y"]))
      -- Output: body is wrapped in local let
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (apply (var "f") (var "x"))
          (list [var "_hoist__body_1", var "y"]))),

    hoistCase "hoistApplications: application in record field is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in {value: f x}
      (letExpr "x" (int32 1)
        (record (nm "Data") [(nm "value", apply (var "f") (var "x"))]))
      -- Output: body is wrapped in local let
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (apply (var "f") (var "x"))
          (record (nm "Data") [(nm "value", var "_hoist__body_1")]))),

    hoistCase "hoistApplications: nested applications hoisted from inside out"
      hoistPredicateApplications
      -- Input: let x = 1 in [f (g x)]
      (letExpr "x" (int32 1)
        (list [apply (var "f") (apply (var "g") (var "x"))]))
      -- Output: inner application hoisted first, then outer
      (letExpr "x" (int32 1)
        (multiLet [
          ("_hoist__body_1", apply (var "g") (var "x")),
          ("_hoist__body_2", apply (var "f") (var "_hoist__body_1"))]
          (list [var "_hoist__body_2"]))),

    -- ============================================================
    -- Test: hoistCaseStatements predicate
    -- Hoists case/match statements.
    -- ============================================================

    hoistCase "hoistCaseStatements: case in application argument is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = just 42 in f (match x with just y -> y | nothing -> 0)
      (letExpr "x" (optional $ just $ int32 42)
        (apply (var "f")
          (match (nm "Optional") (just $ var "x")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])))
      -- Output: body is wrapped in local let with hoisted case
      (letExpr "x" (optional $ just $ int32 42)
        (letExpr "_hoist__body_1"
          (match (nm "Optional") (just $ var "x")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (apply (var "f") (var "_hoist__body_1")))),

    hoistCase "hoistCaseStatements: case in list element is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = 1 in [match y with ok -> x | err -> 0]
      (letExpr "x" (int32 1)
        (list [match (nm "Result") (just $ var "y")
          [(nm "ok", var "x"),
           (nm "err", int32 0)]]))
      -- Output: body is wrapped in local let
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1"
          (match (nm "Result") (just $ var "y")
            [(nm "ok", var "x"),
             (nm "err", int32 0)])
          (list [var "_hoist__body_1"]))),

    -- ============================================================
    -- Test: Nested let expressions
    -- Each let is processed independently; inner lets are processed first.
    -- ============================================================

    hoistCase "hoistLists: nested let - inner let processed independently"
      hoistPredicateLists
      -- Input: let x = 1 in (let y = 2 in f [x, y])
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (apply (var "f") (list [var "x", var "y"]))))
      -- Output: the list is hoisted in the inner let's body
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (letExpr "_hoist__body_1" (list [var "x", var "y"])
            (apply (var "f") (var "_hoist__body_1"))))),

    -- ============================================================
    -- Test: Non-let terms are unchanged
    -- hoistSubterms only processes let expressions.
    -- ============================================================

    hoistCase "hoistLists: non-let term is unchanged"
      hoistPredicateLists
      -- Input: f [1, 2, 3] (no enclosing let)
      (apply (var "f") (list [int32 1, int32 2, int32 3]))
      -- Output: unchanged - no let to hoist into
      (apply (var "f") (list [int32 1, int32 2, int32 3])),

    hoistCase "hoistApplications: bare application unchanged"
      hoistPredicateApplications
      -- Input: f (g x) (no enclosing let)
      (apply (var "f") (apply (var "g") (var "x")))
      -- Output: unchanged
      (apply (var "f") (apply (var "g") (var "x"))),

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
      (letExpr "x" (int32 1)
        (apply (var "f") (list [var "x", int32 2])))
      -- Output: list is hoisted without any lambda wrapping
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (list [var "x", int32 2])
          (apply (var "f") (var "_hoist__body_1")))),

    -- Case 2: Hoisted term refers to lambda-bound variable ABOVE the let (no capture needed)
    hoistCase "hoistLists: term referring to lambda above let needs no capture"
      hoistPredicateLists
      -- Input: \y -> let x = 1 in f [y, x]
      -- y is lambda-bound above the let, so it's not in the immediate subterm's scope
      (lambda "y"
        (letExpr "x" (int32 1)
          (apply (var "f") (list [var "y", var "x"]))))
      -- Output: list is hoisted without lambda wrapping (y was bound before let)
      (lambda "y"
        (letExpr "x" (int32 1)
          (letExpr "_hoist__body_1" (list [var "y", var "x"])
            (apply (var "f") (var "_hoist__body_1"))))),

    -- Case 3: Lambda-bound variable between let and hoisted term, but NOT free in hoisted term
    hoistCase "hoistLists: lambda-bound var not free in hoisted term needs no capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, 2])
      -- y is lambda-bound between let and list, but y does not appear in the list [x, 2]
      -- So [x, 2] should be hoisted without capturing y
      (letExpr "x" (int32 1)
        (lambda "y" (apply (var "f") (list [var "x", int32 2]))))
      -- Output: list [x, 2] is hoisted without lambda wrapping for y (y not free in list)
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (list [var "x", int32 2])
          (lambda "y" (apply (var "f") (var "_hoist__body_1"))))),

    -- Case 4: Lambda-bound variable between let and hoisted term, IS free in hoisted term
    hoistCase "hoistLists: lambda-bound var free in hoisted term requires capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, y])
      -- y is lambda-bound between let and list, and y appears in the list [x, y]
      -- So [x, y] should be hoisted with y captured
      (letExpr "x" (int32 1)
        (lambda "y" (apply (var "f") (list [var "x", var "y"]))))
      -- Output: _hoist__body_1 = \y -> [x, y], reference becomes _hoist__body_1 y
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (lambda "y" (list [var "x", var "y"]))
          (lambda "y" (apply (var "f") (apply (var "_hoist__body_1") (var "y")))))),

    -- Case 5: Multiple lambda-bound variables, only some free in hoisted term
    hoistCase "hoistLists: only free lambda-bound vars are captured"
      hoistPredicateLists
      -- Input: let x = 1 in (\a -> \b -> f [x, b])
      -- Both a and b are lambda-bound between let and list
      -- But only b appears in the list [x, b], so only b is captured
      (letExpr "x" (int32 1)
        (lambda "a" (lambda "b" (apply (var "f") (list [var "x", var "b"])))))
      -- Output: _hoist__body_1 = \b -> [x, b], reference becomes _hoist__body_1 b
      (letExpr "x" (int32 1)
        (letExpr "_hoist__body_1" (lambda "b" (list [var "x", var "b"]))
          (lambda "a" (lambda "b" (apply (var "f") (apply (var "_hoist__body_1") (var "b"))))))),

    -- ============================================================
    -- Test: Stable naming for sibling immediate subterms
    -- Each sibling uses its parent binding name as a prefix, ensuring
    -- that changes to one sibling don't affect the names in another.
    -- ============================================================

    hoistCase "hoistLists: stable naming for binding and body"
      hoistPredicateLists
      -- Input: let x = f [1, 2] in g [3, 4]
      -- Both binding value and body have lists to hoist
      (letExpr "x" (apply (var "f") (list [int32 1, int32 2]))
                   (apply (var "g") (list [int32 3, int32 4])))
      -- Output: binding uses _hoist_x_1, body uses _hoist__body_1
      (letExpr "x"
        (letExpr "_hoist_x_1" (list [int32 1, int32 2])
          (apply (var "f") (var "_hoist_x_1")))
        (letExpr "_hoist__body_1" (list [int32 3, int32 4])
          (apply (var "g") (var "_hoist__body_1")))),

    hoistCase "hoistLists: stable naming for multiple bindings"
      hoistPredicateLists
      -- Input: let x = f [1]; y = g [2] in x
      (multiLet [
        ("x", apply (var "f") (list [int32 1])),
        ("y", apply (var "g") (list [int32 2]))]
        (var "x"))
      -- Output: each binding uses its own name as prefix (_hoist_x_1, _hoist_y_1)
      (multiLet [
        ("x", letExpr "_hoist_x_1" (list [int32 1])
                (apply (var "f") (var "_hoist_x_1"))),
        ("y", letExpr "_hoist_y_1" (list [int32 2])
                (apply (var "g") (var "_hoist_y_1")))]
        (var "x")),

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
        (lambda "x" (apply (apply (var "pair") (apply (var "f") (var "x")))
                               (list [var "x", int32 1])))
        (apply (var "f") (int32 42)))
      -- Output: the list is hoisted into a local let within f's binding value
      (letExpr "f"
        (letExpr "_hoist_f_1" (lambda "x" (list [var "x", int32 1]))
          (lambda "x" (apply (apply (var "pair") (apply (var "f") (var "x")))
                                 (apply (var "_hoist_f_1") (var "x")))))
        (apply (var "f") (int32 42)))]

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
      (letExpr "x" (optional $ just $ int32 42)
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "y" (var "y")),
           (nm "nothing", int32 0)]))
      -- Output: unchanged - case is at top level
      (letExpr "x" (optional $ just $ int32 42)
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "y" (var "y")),
           (nm "nothing", int32 0)])),

    hoistCaseStatementsCase "case in let binding value is NOT hoisted"
      -- Input: let x = match y with just z -> z | nothing -> 0 in x
      -- The case statement is at top level of binding value
      (letExpr "x"
        (match (nm "Optional") (just $ var "y")
          [(nm "just", lambda "z" (var "z")),
           (nm "nothing", int32 0)])
        (var "x"))
      -- Output: unchanged - case is at top level
      (letExpr "x"
        (match (nm "Optional") (just $ var "y")
          [(nm "just", lambda "z" (var "z")),
           (nm "nothing", int32 0)])
        (var "x")),

    hoistCaseStatementsCase "case inside lambda body is NOT hoisted"
      -- Input: let f = \a -> match a with just y -> y | nothing -> 0 in f (just 42)
      -- The case is inside a lambda body, but lambda bodies are pass-through
      -- This becomes def f(a): match a: ... in Python
      (letExpr "f"
        (lambda "a"
          (match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (apply (var "f") (optional $ just $ int32 42)))
      -- Output: unchanged - case is at top level (through lambda body)
      (letExpr "f"
        (lambda "a"
          (match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (apply (var "f") (optional $ just $ int32 42))),

    hoistCaseStatementsCase "case inside nested lambdas is NOT hoisted"
      -- Input: let f = \a -> \b -> match a with ok -> b | err -> 0 in f
      -- The case is inside nested lambdas - still at top level
      -- This becomes def f(a, b): match a: ... in Python
      (letExpr "f"
        (lambda "a" (lambda "b"
          (match (nm "Result") (just $ var "a")
            [(nm "ok", var "b"),
             (nm "err", int32 0)])))
        (var "f"))
      -- Output: unchanged - case is at top level (through lambda bodies)
      (letExpr "f"
        (lambda "a" (lambda "b"
          (match (nm "Result") (just $ var "a")
            [(nm "ok", var "b"),
             (nm "err", int32 0)])))
        (var "f")),

    hoistCaseStatementsCase "case as LHS of one application is NOT hoisted"
      -- Input: let f = (match Optional with ...) x in f
      -- The case is LHS of one application - still at top level (one app LHS allowed)
      -- This is match taking its single argument
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (var "x"))
        (var "f"))
      -- Output: unchanged - case is at top level (one application LHS)
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (var "x"))
        (var "f")),

    hoistCaseStatementsCase "case wrapped in annotation is NOT hoisted"
      -- Input: let f = @ann (match Optional with ...) in f
      -- The case is wrapped in annotation - annotations are transparent
      (letExpr "f"
        (annot emptyAnnMap
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (var "f"))
      -- Output: unchanged - case is at top level (through annotation)
      (letExpr "f"
        (annot emptyAnnMap
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (var "f")),

    hoistCaseStatementsCase "case in lambda with one application is NOT hoisted"
      -- Input: let f = \a -> (match Optional with ...) a in f
      -- Lambda body + one application LHS = still at top level
      (letExpr "f"
        (lambda "a"
          (apply
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (var "a")))
        (var "f"))
      -- Output: unchanged - case is at top level
      (letExpr "f"
        (lambda "a"
          (apply
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (var "a")))
        (var "f")),

    -- ============================================================
    -- Test: Case statement NOT at top level - SHOULD be hoisted
    -- ============================================================

    hoistCaseStatementsCase "case as RHS of application IS hoisted"
      -- Input: let f = g (match Optional with ...) in f
      -- The case is RHS of application (argument position) - NOT top level
      (letExpr "f"
        (apply (var "g")
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (apply (var "g") (var "_hoist_f_1")))
        (var "f")),

    hoistCaseStatementsCase "case in nested application LHS IS hoisted"
      -- Input: let f = ((match Optional with ...) x) y in f
      -- The case is LHS of LHS of application - only ONE app LHS allowed
      -- The second application takes us out of top level
      (letExpr "f"
        (apply
          (apply
            (match (nm "Optional") nothing
              [(nm "just", lambda "z" (lambda "w" (var "z"))),
               (nm "nothing", lambda "w" (int32 0))])
            (var "x"))
          (var "y"))
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "z" (lambda "w" (var "z"))),
             (nm "nothing", lambda "w" (int32 0))])
          (apply (apply (var "_hoist_f_1") (var "x")) (var "y")))
        (var "f")),

    hoistCaseStatementsCase "case inside list element IS hoisted"
      -- Input: let f = [match Optional with ...] in f
      -- The case is inside a list element - NOT top level
      (letExpr "f"
        (list [match (nm "Optional") nothing
          [(nm "just", lambda "y" (var "y")),
           (nm "nothing", int32 0)]])
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (list [var "_hoist_f_1"]))
        (var "f")),

    hoistCaseStatementsCase "case inside lambda inside list IS hoisted"
      -- Input: let f = [\a -> match a with ...] in f
      -- Even though case is inside lambda, the lambda itself is inside a list
      -- The list position makes it not top level
      (letExpr "f"
        (list [lambda "a"
          (match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])])
        (var "f"))
      -- Output: case is hoisted with 'a' captured
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (lambda "a"
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)]))
          (list [lambda "a" (apply (var "_hoist_f_1") (var "a"))]))
        (var "f")),

    -- ============================================================
    -- Test: Non-case terms - should NOT be hoisted regardless
    -- ============================================================

    hoistCaseStatementsCase "list inside lambda is NOT hoisted (only case statements)"
      -- Input: let f = \a -> [a, 1, 2] in f 0
      -- The list is not at top level, but hoistCaseStatements only hoists cases
      (letExpr "f"
        (lambda "a" (list [var "a", int32 1, int32 2]))
        (apply (var "f") (int32 0)))
      -- Output: unchanged - only case statements are hoisted
      (letExpr "f"
        (lambda "a" (list [var "a", int32 1, int32 2]))
        (apply (var "f") (int32 0))),

    -- ============================================================
    -- Test: Mixed scenarios
    -- ============================================================

    hoistCaseStatementsCase "case in binding is not hoisted, case in arg position is hoisted"
      -- Input: let x = match a with ... in f (match b with ...)
      -- First case is at top level of binding, second is in argument position
      (letExpr "x"
        (match (nm "Optional") (just $ var "a")
          [(nm "just", lambda "z" (var "z")),
           (nm "nothing", int32 0)])
        (apply (var "f")
          (match (nm "Optional") (just $ var "b")
            [(nm "just", lambda "w" (var "w")),
             (nm "nothing", int32 0)])))
      -- Output: only second case is hoisted
      (letExpr "x"
        (match (nm "Optional") (just $ var "a")
          [(nm "just", lambda "z" (var "z")),
           (nm "nothing", int32 0)])
        (letExpr "_hoist__body_1"
          (match (nm "Optional") (just $ var "b")
            [(nm "just", lambda "w" (var "w")),
             (nm "nothing", int32 0)])
          (apply (var "f") (var "_hoist__body_1")))),

    -- ============================================================
    -- Test: Mixed let and lambda at top level (no hoisting needed)
    -- ============================================================

    hoistCaseStatementsCase "case in nested let body is NOT hoisted"
      -- Input: let x = 1 in let y = 2 in match z with ...
      -- The case is in nested let body - still at top level
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (match (nm "Optional") (just $ var "z")
            [(nm "just", lambda "w" (var "w")),
             (nm "nothing", int32 0)])))
      -- Output: unchanged
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (match (nm "Optional") (just $ var "z")
            [(nm "just", lambda "w" (var "w")),
             (nm "nothing", int32 0)]))),

    hoistCaseStatementsCase "case in let inside lambda is NOT hoisted"
      -- Input: let f = \a -> let x = 1 in match a with ...
      -- Lambda body then let body - both pass through
      (letExpr "f"
        (lambda "a"
          (letExpr "x" (int32 1)
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])))
        (var "f"))
      -- Output: unchanged
      (letExpr "f"
        (lambda "a"
          (letExpr "x" (int32 1)
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])))
        (var "f")),

    hoistCaseStatementsCase "case in lambda inside let body is NOT hoisted"
      -- Input: let x = 1 in \a -> match a with ...
      -- Let body then lambda body - both pass through
      (letExpr "x" (int32 1)
        (lambda "a"
          (match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])))
      -- Output: unchanged
      (letExpr "x" (int32 1)
        (lambda "a"
          (match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))),

    hoistCaseStatementsCase "case with let+lambda+app is NOT hoisted"
      -- Input: let f = \a -> let x = 1 in (match a with ...) x
      -- Lambda body, let body, one app LHS - all pass through
      (letExpr "f"
        (lambda "a"
          (letExpr "x" (int32 1)
            (apply
              (match (nm "Optional") nothing
                [(nm "just", lambda "y" (var "y")),
                 (nm "nothing", int32 0)])
              (var "x"))))
        (var "f"))
      -- Output: unchanged
      (letExpr "f"
        (lambda "a"
          (letExpr "x" (int32 1)
            (apply
              (match (nm "Optional") nothing
                [(nm "just", lambda "y" (var "y")),
                 (nm "nothing", int32 0)])
              (var "x"))))
        (var "f")),

    -- ============================================================
    -- Test: Multiple applications (hoisting required)
    -- ============================================================

    hoistCaseStatementsCase "case in triple application LHS IS hoisted"
      -- Input: let f = (((match ...) x) y) z in f
      -- Three nested applications - only one app LHS allowed
      (letExpr "f"
        (apply
          (apply
            (apply
              (match (nm "Optional") nothing
                [(nm "just", lambda "a" (lambda "b" (lambda "c" (var "a")))),
                 (nm "nothing", lambda "b" (lambda "c" (int32 0)))])
              (var "x"))
            (var "y"))
          (var "z"))
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "a" (lambda "b" (lambda "c" (var "a")))),
             (nm "nothing", lambda "b" (lambda "c" (int32 0)))])
          (apply (apply (apply (var "_hoist_f_1") (var "x")) (var "y")) (var "z")))
        (var "f")),

    hoistCaseStatementsCase "case as second argument IS hoisted"
      -- Input: let f = g x (match ...) in f
      -- Case is RHS of second application
      (letExpr "f"
        (apply (apply (var "g") (var "x"))
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (apply (apply (var "g") (var "x")) (var "_hoist_f_1")))
        (var "f")),

    hoistCaseStatementsCase "case in both arguments - both hoisted"
      -- Input: let f = g (match a ...) (match b ...) in f
      -- Both cases are in argument positions
      (letExpr "f"
        (apply
          (apply (var "g")
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "x" (var "x")),
               (nm "nothing", int32 0)]))
          (match (nm "Optional") (just $ var "b")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 1)]))
        (var "f"))
      -- Output: both cases hoisted into a SINGLE let with two bindings
      -- (hoistSubterms collects all hoistable terms from one subterm into one let)
      (letExpr "f"
        (lets
          [(nm "_hoist_f_1",
            match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "x" (var "x")),
               (nm "nothing", int32 0)]),
           (nm "_hoist_f_2",
            match (nm "Optional") (just $ var "b")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 1)])]
          (apply (apply (var "g") (var "_hoist_f_1")) (var "_hoist_f_2")))
        (var "f")),

    -- ============================================================
    -- Test: Descent into various structures (hoisting required)
    -- ============================================================

    hoistCaseStatementsCase "case in second list element IS hoisted"
      -- Input: let f = [1, match ...] in f
      (letExpr "f"
        (list [int32 1,
          match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]])
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (list [int32 1, var "_hoist_f_1"]))
        (var "f")),

    hoistCaseStatementsCase "multiple cases in list - all hoisted"
      -- Input: let f = [match a ..., match b ...] in f
      (letExpr "f"
        (list [
          match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "x" (var "x")),
             (nm "nothing", int32 0)],
          match (nm "Optional") (just $ var "b")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 1)]])
        (var "f"))
      -- Output: both cases hoisted into a SINGLE let with two bindings
      (letExpr "f"
        (lets
          [(nm "_hoist_f_1",
            match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "x" (var "x")),
               (nm "nothing", int32 0)]),
           (nm "_hoist_f_2",
            match (nm "Optional") (just $ var "b")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 1)])]
          (list [var "_hoist_f_1", var "_hoist_f_2"]))
        (var "f")),

    hoistCaseStatementsCase "case in pair first element IS hoisted"
      -- Input: let f = (match ..., 1) in f
      (letExpr "f"
        (pair
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (int32 1))
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (pair (var "_hoist_f_1") (int32 1)))
        (var "f")),

    hoistCaseStatementsCase "case in pair second element IS hoisted"
      -- Input: let f = (1, match ...) in f
      (letExpr "f"
        (pair
          (int32 1)
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)]))
        (var "f"))
      -- Output: case is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (pair (int32 1) (var "_hoist_f_1")))
        (var "f")),

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
          (apply (var "g")
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)]))
          (var "inner"))
        (var "outer"))
      -- Output: case hoisted into inner let's binding
      (letExpr "outer"
        (letExpr "inner"
          (letExpr "_hoist_inner_1"
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (apply (var "g") (var "_hoist_inner_1")))
          (var "inner"))
        (var "outer")),

    hoistCaseStatementsCase "case in child let body hoisted into child"
      -- Input: let outer = (let inner = 1 in g (match ...)) in outer
      -- The case is in the body of inner let, inside argument position
      -- It should be hoisted into the inner let's body, not the outer
      (letExpr "outer"
        (letExpr "inner" (int32 1)
          (apply (var "g")
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])))
        (var "outer"))
      -- Output: case hoisted into inner let's body
      (letExpr "outer"
        (letExpr "inner" (int32 1)
          (letExpr "_hoist__body_1"
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (apply (var "g") (var "_hoist__body_1"))))
        (var "outer")),

    hoistCaseStatementsCase "case at top level of child let NOT hoisted"
      -- Input: let outer = (let inner = match ... in inner) in outer
      -- The case is at top level of inner let's binding - no hoisting needed
      (letExpr "outer"
        (letExpr "inner"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (var "inner"))
        (var "outer"))
      -- Output: unchanged
      (letExpr "outer"
        (letExpr "inner"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (var "inner"))
        (var "outer")),

    hoistCaseStatementsCase "cases in both outer and child - each hoisted locally"
      -- Input: let outer = f (match a ...) (let inner = g (match b ...) in inner) in outer
      -- First case in outer's body (arg position), second in inner's binding (arg position)
      -- Each should be hoisted into its respective scope
      -- Each binding uses its name as prefix: inner gets _hoist_inner_1, outer gets _hoist_outer_1
      (letExpr "outer"
        (apply
          (apply (var "f")
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "x" (var "x")),
               (nm "nothing", int32 0)]))
          (letExpr "inner"
            (apply (var "g")
              (match (nm "Optional") (just $ var "b")
                [(nm "just", lambda "y" (var "y")),
                 (nm "nothing", int32 1)]))
            (var "inner")))
        (var "outer"))
      -- Output: outer binding gets _hoist_outer_1, inner binding gets _hoist_inner_1
      (letExpr "outer"
        (letExpr "_hoist_outer_1"
          (match (nm "Optional") (just $ var "a")
            [(nm "just", lambda "x" (var "x")),
             (nm "nothing", int32 0)])
          (apply
            (apply (var "f") (var "_hoist_outer_1"))
            (letExpr "inner"
              (letExpr "_hoist_inner_1"
                (match (nm "Optional") (just $ var "b")
                  [(nm "just", lambda "y" (var "y")),
                   (nm "nothing", int32 1)])
                (apply (var "g") (var "_hoist_inner_1")))
              (var "inner"))))
        (var "outer")),

    -- ============================================================
    -- Test: Lambda after app LHS - should trigger hoisting
    -- ============================================================

    hoistCaseStatementsCase "lambda after app LHS takes us out of top level"
      -- Input: let f = ((\a -> match a with ...) x) in f
      -- App LHS then lambda body - after using app, lambda doesn't help
      (letExpr "f"
        (apply
          (lambda "a"
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)]))
          (var "x"))
        (var "f"))
      -- Output: case is hoisted because lambda comes after we've used our one app LHS
      -- Path to case: letBinding, applicationFunction, lambdaBody
      -- Processing: letBinding (pass), applicationFunction (use app, mark usedApp=true), lambdaBody (usedApp=true, fail)
      -- So case IS hoisted. The case uses 'a' which is lambda-bound, so it's wrapped in a lambda
      -- and the reference becomes (_hoist_f_1 a)
      (letExpr "f"
        (letExpr "_hoist_f_1"
          -- The hoisted case is wrapped in a lambda to capture 'a'
          (lambda "a"
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)]))
          -- The original lambda body is replaced with (_hoist_f_1 a)
          (apply
            (lambda "a" (apply (var "_hoist_f_1") (var "a")))
            (var "x")))
        (var "f")),

    -- ============================================================
    -- Test: Case statements inside case branches (nested cases)
    -- Case branches bind variables, so they are like lambda bodies
    -- ============================================================

    hoistCaseStatementsCase "case inside case branch is NOT hoisted"
      -- Input: let f = match x with just a -> match a with ... | nothing -> 0 in f
      -- Inner case is inside a case branch - branches are pass-through like lambdas
      (letExpr "f"
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "a"
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "b" (var "b")),
               (nm "nothing", int32 0)])),
           (nm "nothing", int32 0)])
        (var "f"))
      -- Output: unchanged - inner case is at top level (through case branch)
      (letExpr "f"
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "a"
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "b" (var "b")),
               (nm "nothing", int32 0)])),
           (nm "nothing", int32 0)])
        (var "f")),

    hoistCaseStatementsCase "case inside case default branch is NOT hoisted"
      -- Input: let f = match x with just a -> a | nothing -> match y with ... in f
      -- Inner case is in default branch - still pass-through
      (letExpr "f"
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "a" (var "a")),
           (nm "nothing",
            match (nm "Optional") (just $ var "y")
              [(nm "just", lambda "b" (var "b")),
               (nm "nothing", int32 0)])])
        (var "f"))
      -- Output: unchanged - inner case is at top level (through case default)
      (letExpr "f"
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "a" (var "a")),
           (nm "nothing",
            match (nm "Optional") (just $ var "y")
              [(nm "just", lambda "b" (var "b")),
               (nm "nothing", int32 0)])])
        (var "f")),

    hoistCaseStatementsCase "case in arg position inside case branch IS hoisted"
      -- Input: let f = match x with just a -> g (match a with ...) | nothing -> 0 in f
      -- Inner case is in argument position inside a case branch - should be hoisted
      (letExpr "f"
        (match (nm "Optional") (just $ var "x")
          [(nm "just", lambda "a"
            (apply (var "g")
              (match (nm "Optional") (just $ var "a")
                [(nm "just", lambda "b" (var "b")),
                 (nm "nothing", int32 0)]))),
           (nm "nothing", int32 0)])
        (var "f"))
      -- Output: inner case is hoisted to the OUTER let level (not inside the branch)
      -- because hoistSubterms only creates lets at existing let boundaries.
      -- The inner case uses 'a' which is lambda-bound, so it's wrapped in a lambda
      -- and the reference becomes (_hoist_f_1 a)
      (letExpr "f"
        (letExpr "_hoist_f_1"
          -- The inner case wrapped in a lambda to capture 'a'
          (lambda "a"
            (match (nm "Optional") (just $ var "a")
              [(nm "just", lambda "b" (var "b")),
               (nm "nothing", int32 0)]))
          -- The outer case with the reference (_hoist_f_1 a) inside the branch
          (match (nm "Optional") (just $ var "x")
            [(nm "just", lambda "a"
              (apply (var "g") (apply (var "_hoist_f_1") (var "a")))),
             (nm "nothing", int32 0)]))
        (var "f"))]

-- | Test cases for hoistPolymorphicLetBindings
-- This function hoists polymorphic let bindings (those with non-empty type scheme variables)
-- to the top level of the given let term.
hoistPolymorphicLetBindingsGroup :: TTerm TestGroup
hoistPolymorphicLetBindingsGroup = subgroup "hoistPolymorphicLetBindings" [
    -- ============================================================
    -- Test: No polymorphic bindings - unchanged
    -- ============================================================

    hoistPolyCase "no polymorphic bindings: simple let unchanged"
      -- Input: let x : Int32 = 42 in x
      (mkLet [(nm "x", int32 42, monoType T.int32)] (var "x"))
      -- Output: unchanged
      (mkLet [(nm "x", int32 42, monoType T.int32)] (var "x")),

    hoistPolyCase "no polymorphic bindings: multiple monomorphic bindings"
      -- Input: let x : Int32 = 1; y : String = "hi" in pair x y
      (mkLet [
        (nm "x", int32 1, monoType T.int32),
        (nm "y", string "hi", monoType T.string)]
        (apply (apply (var "pair") (var "x")) (var "y")))
      -- Output: unchanged
      (mkLet [
        (nm "x", int32 1, monoType T.int32),
        (nm "y", string "hi", monoType T.string)]
        (apply (apply (var "pair") (var "x")) (var "y"))),

    -- ============================================================
    -- Test: Single polymorphic binding at top level - stays in place
    -- ============================================================

    hoistPolyCase "single polymorphic binding: already at top level"
      -- Input: let id : forall a. a -> a = \x -> x in id 42
      (mkLet [(nm "id", lambda "x" (var "x"), polyType ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42)))
      -- Output: unchanged (already at top)
      (mkLet [(nm "id", lambda "x" (var "x"), polyType ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42))),

    -- ============================================================
    -- Test: Polymorphic binding inside lambda body
    -- ============================================================

    hoistPolyCase "polymorphic binding inside lambda: no capture"
      -- Input: let f = \a -> (let id : forall b. b -> b = \x -> x in id a) in f 42
      -- Here 'id' does not reference 'a', so it can be hoisted directly
      (mkLet [(nm "f",
        lambda "a" (Core.termLet $ mkLet [(nm "id", lambda "x" (var "x"), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "id") (var "a"))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "f") (int32 42)))
      -- Output: f comes first (original binding), then f_id is hoisted (no wrapping needed)
      (mkLet [
        (nm "f",
          lambda "a" (apply (var "f_id") (var "a")),
          monoType (T.function T.int32 T.int32)),
        (nm "f_id", tylam "b" (lambda "x" (var "x")), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
        (apply (var "f") (int32 42))),

    -- ============================================================
    -- Test: Polymorphic binding captures lambda variable - requires wrapping
    -- ============================================================

    hoistPolyCase "polymorphic binding captures lambda variable: wrapped in lambda"
      -- Input: let f = \(a:String) -> (let g : forall b. b -> Pair<String, b> = \x -> pair a x in g 42) in f "hello"
      -- Here 'g' references 'a' (the enclosing lambda's variable), so when hoisted,
      -- it must be wrapped in a lambda that takes 'a', and the reference replaced with an application
      (mkLet [(nm "f",
        lambdaTyped "a" T.string (Core.termLet $ mkLet [
          (nm "g", lambda "x" (apply (apply (var "pair") (var "a")) (var "x")),
            polyType ["b"] (T.function (T.var "b") (T.pair T.string (T.var "b"))))]
          (apply (var "g") (int32 42))),
        monoType (T.function T.string (T.pair T.string T.int32)))]
        (apply (var "f") (string "hello")))
      -- Output: f comes first (original binding), then f_g is hoisted wrapped in \(a:String) -> ..., and reference becomes (f_g a)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.string (apply (apply (var "f_g") (var "a")) (int32 42)),
          monoType (T.function T.string (T.pair T.string T.int32))),
        (nm "f_g", tylam "b" (lambdaTyped "a" T.string (lambda "x" (apply (apply (var "pair") (var "a")) (var "x")))),
          polyType ["b"] (T.function T.string (T.function (T.var "b") (T.pair T.string (T.var "b")))))]
        (apply (var "f") (string "hello"))),

    hoistPolyCase "polymorphic binding captures multiple lambda variables"
      -- Input: let f = \(a:Int32) -> \(b:Int32) -> (let g : forall c. c -> c = \x -> triple a b x in g 42) in f 1 2
      -- Here 'g' references both 'a' and 'b', so it needs to be wrapped with both
      (mkLet [(nm "f",
        lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "x" (apply (apply (apply (var "triple") (var "a")) (var "b")) (var "x")),
            polyType ["c"] (T.function (T.var "c") (T.var "c")))]
          (apply (var "g") (int32 42)))),
        monoType (T.function T.int32 (T.function T.int32 T.int32)))]
        (apply (apply (var "f") (int32 1)) (int32 2)))
      -- Output: f comes first (original binding), then f_g is wrapped in \(a:Int32) -> \(b:Int32) -> ..., references become ((f_g a) b)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (apply (apply (apply (var "f_g") (var "a")) (var "b")) (int32 42))),
          monoType (T.function T.int32 (T.function T.int32 T.int32))),
        (nm "f_g", tylam "c" (lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (lambda "x" (apply (apply (apply (var "triple") (var "a")) (var "b")) (var "x"))))),
          polyType ["c"] (T.function T.int32 (T.function T.int32 (T.function (T.var "c") (T.var "c")))))]
        (apply (apply (var "f") (int32 1)) (int32 2))),

    hoistPolyCase "polymorphic binding captures some but not all lambda variables"
      -- Input: let f = \(a:Int32) -> \(b:Int32) -> (let g : forall c. c -> Pair<Int32, c> = \x -> pair a x in g b) in f 1 2
      -- Here 'g' only references 'a', not 'b'
      (mkLet [(nm "f",
        lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "x" (apply (apply (var "pair") (var "a")) (var "x")),
            polyType ["c"] (T.function (T.var "c") (T.pair T.int32 (T.var "c"))))]
          (apply (var "g") (var "b")))),
        monoType (T.function T.int32 (T.function T.int32 (T.pair T.int32 T.int32))))]
        (apply (apply (var "f") (int32 1)) (int32 2)))
      -- Output: f comes first (original binding), then f_g is wrapped only in \(a:Int32) -> ..., reference becomes (f_g a)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (apply (apply (var "f_g") (var "a")) (var "b"))),
          monoType (T.function T.int32 (T.function T.int32 (T.pair T.int32 T.int32)))),
        (nm "f_g", tylam "c" (lambdaTyped "a" T.int32 (lambda "x" (apply (apply (var "pair") (var "a")) (var "x")))),
          polyType ["c"] (T.function T.int32 (T.function (T.var "c") (T.pair T.int32 (T.var "c")))))]
        (apply (apply (var "f") (int32 1)) (int32 2))),

    hoistPolyCase "polymorphic binding captures both lambda-bound and let-bound variables"
      -- Input: let f = \(a:Int32) -> (let x : Int32 = 1; g : forall b. b -> b = \y -> add (add a x) y in g 42) in f 10
      -- Here 'g' references 'a' (lambda-bound from enclosing scope) and 'x' (let-bound sibling).
      -- When g is hoisted to the top level, it goes OUTSIDE the inner let block, so it must capture
      -- both 'a' (from the enclosing lambda) AND 'x' (which is no longer a sibling after hoisting).
      (mkLet [(nm "f",
        lambdaTyped "a" T.int32 (Core.termLet $ mkLet [
          (nm "x", int32 1, monoType T.int32),
          (nm "g", lambda "y" (apply (apply (primitive _math_add)
                    (apply (apply (primitive _math_add) (var "a")) (var "x"))) (var "y")),
            polyType ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "g") (int32 42))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "f") (int32 10)))
      -- Output: f comes first (original binding), then f_g captures both 'a' and 'x', reference becomes ((f_g a) x)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.int32 (Core.termLet $ mkLet [
            (nm "x", int32 1, monoType T.int32)]
            (apply (apply (apply (var "f_g") (var "a")) (var "x")) (int32 42))),
          monoType (T.function T.int32 T.int32)),
        (nm "f_g", tylam "b" (lambdaTyped "a" T.int32 (lambdaTyped "x" T.int32
          (lambda "y" (apply (apply (primitive _math_add)
            (apply (apply (primitive _math_add) (var "a")) (var "x"))) (var "y"))))),
          polyType ["b"] (T.function T.int32 (T.function T.int32 (T.function (T.var "b") (T.var "b")))))]
        (apply (var "f") (int32 10))),

    hoistPolyCase "sibling polymorphic bindings inside lambda: one calls the other"
      -- Input: let wrapper = \outer : Int32 -> let g : forall a. a -> a = \y -> add outer y; h : forall b. b -> b = \z -> g z in h 42 in wrapper 10
      -- This mimics the structure in unifyTypeConstraints where bind and tryBinding are
      -- both inside withConstraint lambda and one calls the other.
      -- g captures 'outer', h calls g which means h must transitively capture 'outer'
      (mkLet [(nm "wrapper",
        lambdaTyped "outer" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "y" (apply (apply (primitive _math_add) (var "outer")) (var "y")),
            polyType ["a"] (T.function (T.var "a") (T.var "a"))),
          (nm "h", lambda "z" (apply (var "g") (var "z")),
            polyType ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "h") (int32 42))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "wrapper") (int32 10)))
      -- Output: wrapper first (original), then wrapper_g and wrapper_h hoisted (both capture outer)
      -- wrapper_g is wrapped: \outer -> \y -> add outer y
      -- wrapper_h is wrapped: \outer -> \z -> (wrapper_g outer) z
      -- The reference to wrapper_g inside wrapper_h becomes (wrapper_g outer)
      (mkLet [
        (nm "wrapper",
          lambdaTyped "outer" T.int32 (apply (apply (var "wrapper_h") (var "outer")) (int32 42)),
          monoType (T.function T.int32 T.int32)),
        (nm "wrapper_g", tylam "a" (lambdaTyped "outer" T.int32 (lambda "y" (apply (apply (primitive _math_add) (var "outer")) (var "y")))),
          polyType ["a"] (T.function T.int32 (T.function (T.var "a") (T.var "a")))),
        (nm "wrapper_h", tylam "b" (lambdaTyped "outer" T.int32 (lambda "z" (apply (apply (var "wrapper_g") (var "outer")) (var "z")))),
          polyType ["b"] (T.function T.int32 (T.function (T.var "b") (T.var "b"))))]
        (apply (var "wrapper") (int32 10))),

    hoistPolyCase "sibling polymorphic bindings inside lambda: h passes its own args to g"
      -- Input: let wrapper = \outer -> let g : forall a. a -> a = \v -> \t -> add outer (add v t);
      --                                    h : forall b. b -> b = \v -> \t -> g v t
      --                                in h 1 2
      --        in wrapper 10
      -- Here h calls g with its own args v and t. After hoisting:
      -- g becomes \outer -> \v -> \t -> add outer (add v t)
      -- h becomes \outer -> \v -> \t -> ((g outer) v) t
      -- The reference to g in h should NOT duplicate outer, v, or t
      (mkLet [(nm "wrapper",
        lambdaTyped "outer" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "v" (lambda "t" (apply (apply (primitive _math_add) (var "outer"))
            (apply (apply (primitive _math_add) (var "v")) (var "t")))),
            polyType ["a"] (T.function (T.var "a") (T.function (T.var "a") (T.var "a")))),
          (nm "h", lambda "v" (lambda "t" (apply (apply (var "g") (var "v")) (var "t"))),
            polyType ["b"] (T.function (T.var "b") (T.function (T.var "b") (T.var "b"))))]
          (apply (apply (var "h") (int32 1)) (int32 2))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "wrapper") (int32 10)))
      -- Output: wrapper first (original), then wrapper_g and wrapper_h hoisted (both capture outer)
      -- wrapper_g: \outer -> \v -> \t -> add outer (add v t)
      -- wrapper_h: \outer -> \v -> \t -> ((wrapper_g outer) v) t
      (mkLet [
        (nm "wrapper",
          lambdaTyped "outer" T.int32 (apply (apply (apply (var "wrapper_h") (var "outer")) (int32 1)) (int32 2)),
          monoType (T.function T.int32 T.int32)),
        (nm "wrapper_g", tylam "a" (lambdaTyped "outer" T.int32
          (lambda "v" (lambda "t" (apply (apply (primitive _math_add) (var "outer"))
            (apply (apply (primitive _math_add) (var "v")) (var "t")))))),
          polyType ["a"] (T.function T.int32 (T.function (T.var "a") (T.function (T.var "a") (T.var "a"))))),
        (nm "wrapper_h", tylam "b" (lambdaTyped "outer" T.int32
          (lambda "v" (lambda "t" (apply (apply (apply (var "wrapper_g") (var "outer")) (var "v")) (var "t"))))),
          polyType ["b"] (T.function T.int32 (T.function (T.var "b") (T.function (T.var "b") (T.var "b")))))]
        (apply (var "wrapper") (int32 10))),

    -- ============================================================
    -- Test: Untyped bindings are treated as monomorphic
    -- ============================================================

    hoistPolyCase "untyped binding: not hoisted"
      -- Input: let x = 1 in (let y = 2 in x + y)
      -- Neither binding has a type, so neither is polymorphic
      (mkLetUntyped [(nm "x", int32 1)]
        (Core.termLet $ mkLetUntyped [(nm "y", int32 2)]
          (apply (apply (primitive _math_add) (var "x")) (var "y"))))
      -- Output: unchanged (untyped bindings are not polymorphic)
      (mkLetUntyped [(nm "x", int32 1)]
        (Core.termLet $ mkLetUntyped [(nm "y", int32 2)]
          (apply (apply (primitive _math_add) (var "x")) (var "y")))),

    -- ============================================================
    -- Test: Name collision - nested binding has same name as top-level (after unshadowing)
    -- Note: This test uses distinct names as unshadowVariables would produce.
    -- The hoisting code assumes no shadowing. If there were shadowing,
    -- two bindings named 'id' would collide at top level.
    -- ============================================================

    hoistPolyCase "no name collision: distinct names after unshadowing"
      -- Input: let id = \x -> x; f = \a -> (let id2 : forall b. b -> b = \y -> y in id2 (id a)) in f 42
      -- Here 'id' is monomorphic at top level, 'id2' is polymorphic nested
      -- They have distinct names (as unshadowVariables would ensure)
      (mkLet [
        (nm "id", lambda "x" (var "x"), monoType (T.function T.int32 T.int32)),
        (nm "f",
          lambda "a" (Core.termLet $ mkLet [
            (nm "id2", lambda "y" (var "y"), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
            (apply (var "id2") (apply (var "id") (var "a")))),
          monoType (T.function T.int32 T.int32))]
        (apply (var "f") (int32 42)))
      -- Output: id and f first (original), then f_id2 hoisted
      (mkLet [
        (nm "id", lambda "x" (var "x"), monoType (T.function T.int32 T.int32)),
        (nm "f",
          lambda "a" (apply (var "f_id2") (apply (var "id") (var "a"))),
          monoType (T.function T.int32 T.int32)),
        (nm "f_id2", tylam "b" (lambda "y" (var "y")), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
        (apply (var "f") (int32 42))),

    -- ============================================================
    -- Test: Nested polymorphic binding calls enclosing polymorphic binding
    -- When a polymorphic binding h at a nested level calls a polymorphic
    -- binding g from an enclosing (but not top) level, g should NOT be
    -- passed as a term argument to h. Instead, h's body should reference
    -- the hoisted g (wrapper_g) directly, since polymorphic let-bound
    -- variables are excluded from capture.
    -- ============================================================

    hoistPolyCase "nested polymorphic binding calls enclosing polymorphic binding"
      -- Input: let wrapper = \outer:Int32 ->
      --          let g : forall a. a -> a = \y -> y
      --          in \inner:Int32 ->
      --            let h : forall b. b -> b = \z -> g z
      --            in h 42
      --        in wrapper 10 20
      -- Here g is polymorphic at an outer let level, and h is polymorphic
      -- at an inner let level. h calls g. When both are hoisted:
      -- - g becomes wrapper_g (no captures needed)
      -- - h becomes wrapper_h, and should reference wrapper_g directly
      --   (NOT receive g as a term parameter)
      (mkLet [(nm "wrapper",
        lambdaTyped "outer" T.int32 (Core.termLet $ mkLet [
          -- g : forall a. a -> a (at outer let level)
          (nm "g",
            lambda "y" (var "y"),
            polyType ["a"] (T.function (T.var "a") (T.var "a")))]
          -- inner lambda
          (lambdaTyped "inner" T.int32 (Core.termLet $ mkLet [
            -- h : forall b. b -> b (at inner let level, calls g)
            (nm "h",
              lambda "z" (apply (var "g") (var "z")),
              polyType ["b"] (T.function (T.var "b") (T.var "b")))]
            (apply (var "h") (int32 42))))),
        monoType (T.function T.int32 (T.function T.int32 T.int32)))]
        (apply (apply (var "wrapper") (int32 10)) (int32 20)))
      -- Output: wrapper simplified, wrapper_g and wrapper_h hoisted.
      -- CRITICAL: wrapper_h references wrapper_g directly, NOT through a parameter.
      -- Neither g nor any enclosing lambda variables are captured by h,
      -- because g is polymorphic (excluded from capture) and h doesn't use outer/inner.
      (mkLet [
        (nm "wrapper",
          lambdaTyped "outer" T.int32
            (lambdaTyped "inner" T.int32
              (apply (var "wrapper_h") (int32 42))),
          monoType (T.function T.int32 (T.function T.int32 T.int32))),
        -- wrapper_g: hoisted, no captures
        (nm "wrapper_g",
          tylam "a" (lambda "y" (var "y")),
          polyType ["a"] (T.function (T.var "a") (T.var "a"))),
        -- wrapper_h: hoisted, references wrapper_g directly (no g parameter)
        (nm "wrapper_h",
          tylam "b" (lambda "z" (apply (var "wrapper_g") (var "z"))),
          polyType ["b"] (T.function (T.var "b") (T.var "b")))]
        (apply (apply (var "wrapper") (int32 10)) (int32 20))),

    -- ============================================================
    -- Test: Polymorphic binding captures monomorphic sibling in same let
    -- This test case demonstrates the bug in joinTypes where:
    -- - sleft/sright are monomorphic bindings (NOT hoisted)
    -- - cannotUnify is polymorphic (HOISTED) and references sleft/sright
    -- When cannotUnify is hoisted, it must capture sleft/sright as parameters
    -- because they are defined in the same let and won't be hoisted with it.
    -- ============================================================

    hoistPolyCase "polymorphic binding captures monomorphic sibling in same let"
      -- This mirrors the joinTypes structure:
      -- let wrapper = \left -> \right ->
      --   let sleft : Int32 = f left;           -- monomorphic, NOT hoisted
      --       sright : Int32 = f right;         -- monomorphic, NOT hoisted
      --       cannotUnify : forall a. a -> a = \x -> add sleft (add sright x)  -- polymorphic, refs sleft/sright
      --   in cannotUnify 42
      -- in wrapper 1 2
      --
      -- When cannotUnify is hoisted, it must capture sleft and sright since they are
      -- defined in the same let and won't be hoisted with it.
      (mkLet [(nm "wrapper",
        lambdaTyped "left" T.int32
          (lambdaTyped "right" T.int32
            (Core.termLet $ mkLet [
              (nm "sleft", apply (var "f") (var "left"), monoType T.int32),
              (nm "sright", apply (var "f") (var "right"), monoType T.int32),
              (nm "cannotUnify",
                lambda "x" (apply (apply (primitive _math_add) (var "sleft"))
                  (apply (apply (primitive _math_add) (var "sright")) (var "x"))),
                polyType ["a"] (T.function (T.var "a") (T.var "a")))]
              (apply (var "cannotUnify") (int32 42)))),
        monoType (T.function T.int32 (T.function T.int32 T.int32)))]
        (apply (apply (var "wrapper") (int32 1)) (int32 2)))
      -- Expected output:
      -- wrapper is simplified, wrapper_cannotUnify is hoisted with sleft and sright captured
      -- wrapper_cannotUnify: forall a. \sleft:Int32 -> \sright:Int32 -> \x -> add sleft (add sright x)
      -- Reference becomes: ((wrapper_cannotUnify sleft) sright) 42
      (mkLet [
        (nm "wrapper",
          lambdaTyped "left" T.int32
            (lambdaTyped "right" T.int32
              (Core.termLet $ mkLet [
                (nm "sleft", apply (var "f") (var "left"), monoType T.int32),
                (nm "sright", apply (var "f") (var "right"), monoType T.int32)]
                (apply (apply (apply (var "wrapper_cannotUnify") (var "sleft")) (var "sright")) (int32 42)))),
          monoType (T.function T.int32 (T.function T.int32 T.int32))),
        (nm "wrapper_cannotUnify",
          tylam "a"
            (lambdaTyped "sleft" T.int32
              (lambdaTyped "sright" T.int32
                (lambda "x" (apply (apply (primitive _math_add) (var "sleft"))
                  (apply (apply (primitive _math_add) (var "sright")) (var "x")))))),
          polyType ["a"] (T.function T.int32 (T.function T.int32 (T.function (T.var "a") (T.var "a")))))]
        (apply (apply (var "wrapper") (int32 1)) (int32 2))),

    -- This test uses NESTED lets (as the DSL generates with <~) rather than a single flat let.
    -- The bug: when poly bindings in different nested lets reference each other,
    -- transitive captured variables are not propagated correctly.
    --
    -- Structure (nested lets, like DSL generates):
    --   wrapper = \left ->
    --     let sleft = left                              -- mono (outermost nested let)
    --     in let cannotUnify = fail sleft              -- poly, refs sleft (middle nested let)
    --        in let joinList = cannotUnify 1           -- poly, refs cannotUnify (innermost nested let)
    --           in joinList
    --
    -- When joinList is hoisted, it captures cannotUnify (from outer let).
    -- When cannotUnify is hoisted, it captures sleft and its replacement becomes: wrapper_cannotUnify sleft
    -- The replacement for cannotUnify gets substituted into joinList's body.
    -- Bug: joinList ends up with free variable sleft because it wasn't captured transitively.
    hoistPolyCase "nested lets: poly binding references poly sibling from outer let"
      (mkLet [(nm "wrapper",
        lambdaTyped "left" T.int32
          -- Outer nested let: sleft (mono)
          (Core.termLet $ mkLet [
            (nm "sleft", var "left", monoType T.int32)]
            -- Middle nested let: cannotUnify (poly, refs sleft)
            (Core.termLet $ mkLet [
              (nm "cannotUnify",
                lambda "x" (apply (apply (primitive _math_add) (var "sleft")) (var "x")),
                polyType ["a"] (T.function (T.var "a") (T.var "a")))]
              -- Inner nested let: joinList (poly, refs cannotUnify)
              (Core.termLet $ mkLet [
                (nm "joinList",
                  lambda "y" (apply (var "cannotUnify") (var "y")),
                  polyType ["b"] (T.function (T.var "b") (T.var "b")))]
                (apply (var "joinList") (int32 42))))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "wrapper") (int32 1)))
      -- Expected: both cannotUnify and joinList are hoisted.
      -- wrapper_cannotUnify captures sleft.
      -- wrapper_joinList must ALSO capture sleft (transitively through cannotUnify).
      -- The reference to cannotUnify in joinList becomes wrapper_cannotUnify sleft,
      -- so joinList needs sleft in scope.
      (mkLet [
        (nm "wrapper",
          lambdaTyped "left" T.int32
            (Core.termLet $ mkLet [
              (nm "sleft", var "left", monoType T.int32)]
              (apply (apply (var "wrapper_joinList") (var "sleft")) (int32 42))),
          monoType (T.function T.int32 T.int32)),
        (nm "wrapper_cannotUnify",
          tylam "a"
            (lambdaTyped "sleft" T.int32
              (lambda "x" (apply (apply (primitive _math_add) (var "sleft")) (var "x")))),
          polyType ["a"] (T.function T.int32 (T.function (T.var "a") (T.var "a")))),
        (nm "wrapper_joinList",
          tylam "b"
            (lambdaTyped "sleft" T.int32
              (lambda "y" (apply (apply (var "wrapper_cannotUnify") (var "sleft")) (var "y")))),
          polyType ["b"] (T.function T.int32 (T.function (T.var "b") (T.var "b"))))]
        (apply (var "wrapper") (int32 1)))]

-- | Test cases for hoistLetBindings with hoistAll=True
-- This function hoists ALL let bindings (not just polymorphic ones) to the top level.
-- This is used for Java which cannot have let expressions in arbitrary positions.
-- Key behavior: type lambdas are boundaries - we don't hoist bindings OUT of type lambdas
-- because doing so could move code that references type variables outside their scope.
hoistLetBindingsGroup :: TTerm TestGroup
hoistLetBindingsGroup = subgroup "hoistLetBindings" [
    -- ============================================================
    -- Test: Basic nested let hoisting (no type lambdas)
    -- ============================================================

    hoistAllCase "nested let inside lambda: binding hoisted with lambda capture"
      -- Input: let f = \a -> (let g = a + 1 in g * 2) in f 10
      (mkLetUntyped [(nm "f",
        lambda "a" (Core.termLet $ mkLetUntyped [(nm "g", apply (apply (primitive _math_add) (var "a")) (int32 1))]
          (apply (apply (primitive _math_mul) (var "g")) (int32 2))))]
        (apply (var "f") (int32 10)))
      -- Output: f comes first (original binding), then f_g is hoisted with lambda to capture 'a', reference becomes (f_g a)
      (mkLetUntyped [
        (nm "f",
          lambda "a" (apply (apply (primitive _math_mul) (apply (var "f_g") (var "a"))) (int32 2))),
        (nm "f_g", lambda "a" (apply (apply (primitive _math_add) (var "a")) (int32 1)))]
        (apply (var "f") (int32 10))),

    -- ============================================================
    -- Test: Type applications are processed normally (they don't introduce type variables)
    -- But the inner lambda still has nested let that gets hoisted
    -- ============================================================

    hoistAllCase "type application: nested let outside lambda CAN be hoisted"
      -- Input: let f = (let y = 1 in \x -> x + y) @Int32 in f 10
      -- The let is OUTSIDE the lambda, so y can be hoisted without capture
      (mkLetUntyped [(nm "f",
        tyapp (Core.termLet $ mkLetUntyped [(nm "y", int32 1)]
          (lambda "x" (apply (apply (primitive _math_add) (var "x")) (var "y")))) T.int32)]
        (apply (var "f") (int32 10)))
      -- Output: f comes first (original binding), then f_y is hoisted
      (mkLetUntyped [
        (nm "f",
          tyapp (lambda "x" (apply (apply (primitive _math_add) (var "x")) (var "f_y"))) T.int32),
        (nm "f_y", int32 1)]
        (apply (var "f") (int32 10)))]

-- | Convenience function for creating hoist let bindings test cases (hoistAll=True)
hoistAllCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistAllCase cname input output = hoistLetBindingsCase cname input output

-- | Helper for creating a Let term with typed bindings
mkLet :: [(TTerm Name, TTerm Term, TTerm (Maybe TypeScheme))] -> TTerm Term -> TTerm Let
mkLet bindings body = Core.let_ (Phantoms.list $ mkBinding <$> bindings) body
  where
    mkBinding :: (TTerm Name, TTerm Term, TTerm (Maybe TypeScheme)) -> TTerm Binding
    mkBinding (n, t, ts) = Core.binding n t ts

-- | Helper for creating a Let term with untyped bindings
mkLetUntyped :: [(TTerm Name, TTerm Term)] -> TTerm Term -> TTerm Let
mkLetUntyped bindings body = Core.let_ (Phantoms.list $ mkBinding <$> bindings) body
  where
    mkBinding :: (TTerm Name, TTerm Term) -> TTerm Binding
    mkBinding (n, t) = Core.binding n t Phantoms.nothing

-- | Helper for creating a monomorphic type scheme
monoType :: TTerm Type -> TTerm (Maybe TypeScheme)
monoType typ = Phantoms.just $ Core.typeScheme (Phantoms.list ([] :: [TTerm Name])) typ Phantoms.nothing

-- | Helper for creating a polymorphic type scheme
polyType :: [String] -> TTerm Type -> TTerm (Maybe TypeScheme)
polyType vars typ = Phantoms.just $ Core.typeScheme (Phantoms.list $ nm <$> vars) typ Phantoms.nothing

-- | Convenience function for creating hoist polymorphic let bindings test cases
hoistPolyCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistPolyCase cname input output = hoistPolymorphicLetBindingsCase cname input output

-- | Test cases for type parameter extraction when hoisting polymorphic let bindings
-- This group specifically tests scenarios where Java code generation fails because
-- type parameters (t0, t1, t2, etc.) are used in generated code but not declared
-- in method signatures. The root cause is that javaTypeParametersForType doesn't
-- properly collect all free type variables from nested generic types.
--
-- These tests illustrate the EXPECTED behavior after the issue is fixed.
-- Currently, the Java coder fails to generate compilable code for these cases.
hoistPolymorphicTypeParametersGroup :: TTerm TestGroup
hoistPolymorphicTypeParametersGroup = subgroup "hoistPolymorphicTypeParameters" [
    -- ============================================================
    -- Test: Nested polymorphic bindings with multiple type variables
    -- This is the core issue: when a polymorphic binding like `choose`
    -- uses type variables (t0, t1, t2) in nested function types,
    -- those variables must be declared in the hoisted method signature.
    -- ============================================================

    hoistPolyCase "nested function types: all type variables must be declared"
      -- Input: let f = (let choose : forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1 = ... in choose) in f
      -- This simulates the `mutateTrace` scenario where `choose` is a polymorphic
      -- local binding with multiple type parameters in nested function types.
      -- (Simplified to avoid using "Either" as a type variable name)
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "choose",
          lambda "forLeft" (lambda "forRight" (lambda "e"
            (apply (var "forLeft") (var "e")))),
          -- Type: forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.function (T.var "t0") (T.var "t1"))
              (T.function
                (T.function (T.var "t2") (T.var "t1"))
                (T.function (T.var "t0") (T.var "t1")))))]
          (var "choose"),
        monoType (T.function
          (T.function T.string T.int32)
          (T.function
            (T.function T.boolean T.int32)
            (T.function T.string T.int32))))]
        (var "f"))
      -- Output: f first (original), then f_choose hoisted with ALL type parameters preserved
      -- When generating Java, the method signature must declare t0, t1, t2
      (mkLet [
        (nm "f",
          var "f_choose",
          monoType (T.function
            (T.function T.string T.int32)
            (T.function
              (T.function T.boolean T.int32)
              (T.function T.string T.int32)))),
        (nm "f_choose",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "forLeft" (lambda "forRight" (lambda "e"
            (apply (var "forLeft") (var "e"))))))),
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.function (T.var "t0") (T.var "t1"))
              (T.function
                (T.function (T.var "t2") (T.var "t1"))
                (T.function (T.var "t0") (T.var "t1")))))]
        (var "f")),

    -- ============================================================
    -- Test: Type variable in return position only
    -- Type variables appearing only in the return type of a nested
    -- function must still be declared.
    -- ============================================================

    hoistPolyCase "type variable in return position only"
      -- Input: let f = (let returnT : forall t. () -> t = ... in returnT) in f
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "returnT",
          lambda "unit" (var "undefined"),
          polyType ["t"] (T.function T.unit (T.var "t")))]
          (var "returnT"),
        monoType (T.function T.unit T.int32))]
        (var "f"))
      -- Output: f first (original), then f_returnT hoisted with t declared
      (mkLet [
        (nm "f",
          var "f_returnT",
          monoType (T.function T.unit T.int32)),
        (nm "f_returnT",
          tylam "t" (lambda "unit" (var "undefined")),
          polyType ["t"] (T.function T.unit (T.var "t")))]
        (var "f")),

    -- ============================================================
    -- Test: Type variables in deeply nested generic types
    -- The Java coder uses freeVariablesInType but may not recurse
    -- into all nested type structures.
    -- ============================================================

    hoistPolyCase "type variables in deeply nested generics"
      -- Input: let f = (let nested : forall t0 t1 t2. ((t0, t1), t2) -> t0 = ... in nested) in f
      -- Type variables t0, t1, t2 are in nested pair types
      -- (Simplified from original which used List<Map<...>> - those are treated as type vars in tests)
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "nested",
          lambda "x" (var "undefined"),
          -- Type: forall t0 t1 t2. ((t0, t1), t2) -> t0
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.pair (T.pair (T.var "t0") (T.var "t1")) (T.var "t2"))
              (T.var "t0")))]
          (var "nested"),
        monoType (T.function
          (T.pair (T.pair T.string T.int32) T.boolean)
          T.string))]
        (var "f"))
      -- Output: f first (original), then f_nested hoisted with all type parameters t0, t1, t2 declared
      (mkLet [
        (nm "f",
          var "f_nested",
          monoType (T.function
            (T.pair (T.pair T.string T.int32) T.boolean)
            T.string)),
        (nm "f_nested",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "x" (var "undefined")))),
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.pair (T.pair (T.var "t0") (T.var "t1")) (T.var "t2"))
              (T.var "t0")))]
        (var "f")),

    -- ============================================================
    -- Test: Multiple polymorphic bindings that share type variable names
    -- Each binding should independently track its own type variables.
    -- ============================================================

    hoistPolyCase "multiple bindings with overlapping type variable names"
      -- Input: let outer = (let id1 : forall t. t -> t = \x -> x; id2 : forall t. t -> t = \y -> y in pair id1 id2) in outer
      -- Both id1 and id2 use "t" but they are independent
      (mkLet [(nm "outer",
        Core.termLet $ mkLet [
          (nm "id1", lambda "x" (var "x"), polyType ["t"] (T.function (T.var "t") (T.var "t"))),
          (nm "id2", lambda "y" (var "y"), polyType ["t"] (T.function (T.var "t") (T.var "t")))]
          (apply (apply (var "pair") (var "id1")) (var "id2")),
        monoType (T.pair
          (T.function T.int32 T.int32)
          (T.function T.string T.string)))]
        (var "outer"))
      -- Output: outer first (original), then outer_id1 and outer_id2 hoisted (each with their own t parameter)
      (mkLet [
        (nm "outer",
          apply (apply (var "pair") (var "outer_id1")) (var "outer_id2"),
          monoType (T.pair
            (T.function T.int32 T.int32)
            (T.function T.string T.string))),
        (nm "outer_id1", tylam "t" (lambda "x" (var "x")), polyType ["t"] (T.function (T.var "t") (T.var "t"))),
        (nm "outer_id2", tylam "t" (lambda "y" (var "y")), polyType ["t"] (T.function (T.var "t") (T.var "t")))]
        (var "outer")),

    -- ============================================================
    -- Test: Polymorphic binding with captured term variable AND type parameters
    -- This is the combination case: the binding both captures a lambda-bound
    -- term variable AND has type parameters that need to be declared.
    -- ============================================================

    hoistPolyCase "captured variable with type parameters"
      -- Input: let f = \(a:String) -> (let g : forall t. t -> Pair<String, t> = \x -> pair a x in g 42) in f "hello"
      -- Here 'g' captures 'a' AND has type parameter t
      -- When hoisted: g must be wrapped in lambda for 'a', AND method must declare t
      (mkLet [(nm "f",
        lambdaTyped "a" T.string (Core.termLet $ mkLet [
          (nm "g",
            lambda "x" (apply (apply (var "pair") (var "a")) (var "x")),
            polyType ["t"] (T.function (T.var "t") (T.pair T.string (T.var "t"))))]
          (apply (var "g") (int32 42))),
        monoType (T.function T.string (T.pair T.string T.int32)))]
        (apply (var "f") (string "hello")))
      -- Output: f first (original), then f_g hoisted with lambda wrapper for 'a', and t must be declared
      -- Java signature: <t> Function<String, Function<t, Pair<String, t>>> f_g = a -> x -> pair(a, x);
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.string (apply (apply (var "f_g") (var "a")) (int32 42)),
          monoType (T.function T.string (T.pair T.string T.int32))),
        (nm "f_g",
          tylam "t" (lambdaTyped "a" T.string (lambda "x" (apply (apply (var "pair") (var "a")) (var "x")))),
          polyType ["t"] (T.function T.string (T.function (T.var "t") (T.pair T.string (T.var "t")))))]
        (apply (var "f") (string "hello"))),

    -- ============================================================
    -- Test: Short type variable names (the isLambdaBoundVariable heuristic)
    -- The Java coder's isLambdaBoundVariable uses name length <= 4 to identify
    -- type variables. These tests ensure that this heuristic correctly
    -- identifies variables that should become Java type parameters.
    -- ============================================================

    hoistPolyCase "short type variable names are treated as type parameters"
      -- Input: let f = (let g : forall s t v. s -> t -> v = ... in g) in f
      -- s, t, v have length <= 4, so they should be recognized as type parameters
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "g",
          lambda "s" (lambda "t" (var "undefined")),
          polyType ["s", "t", "v"]
            (T.function (T.var "s")
              (T.function (T.var "t") (T.var "v"))))]
          (var "g"),
        monoType (T.function T.int32 (T.function T.string T.boolean)))]
        (var "f"))
      -- Output: f first (original), then f_g hoisted with s, t, v declared as type parameters
      (mkLet [
        (nm "f",
          var "f_g",
          monoType (T.function T.int32 (T.function T.string T.boolean))),
        (nm "f_g",
          tylam "s" (tylam "t" (tylam "v" (lambda "s" (lambda "t" (var "undefined"))))),
          polyType ["s", "t", "v"]
            (T.function (T.var "s")
              (T.function (T.var "t") (T.var "v"))))]
        (var "f")),

    hoistPolyCase "numbered type variables like t0 t1 t2"
      -- Input: let f = (let g : forall t0 t1 t2. t0 -> t1 -> t2 = ... in g) in f
      -- t0, t1, t2 have length <= 4, common pattern in generated code
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "g",
          lambda "x" (lambda "y" (var "undefined")),
          polyType ["t0", "t1", "t2"]
            (T.function (T.var "t0")
              (T.function (T.var "t1") (T.var "t2"))))]
          (var "g"),
        monoType (T.function T.int32 (T.function T.string T.boolean)))]
        (var "f"))
      -- Output: f first (original), then f_g hoisted with t0, t1, t2 declared
      (mkLet [
        (nm "f",
          var "f_g",
          monoType (T.function T.int32 (T.function T.string T.boolean))),
        (nm "f_g",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "x" (lambda "y" (var "undefined"))))),
          polyType ["t0", "t1", "t2"]
            (T.function (T.var "t0")
              (T.function (T.var "t1") (T.var "t2"))))]
        (var "f")),

    -- ============================================================
    -- Test: Complex "choose" pattern from mutateTrace
    -- This directly models the failing code in Monads.java
    -- NOTE: This test uses T.var "Either" which causes "Either" to be
    -- treated as a free type variable. In real code, Either would be a defined
    -- type constructor and wouldn't be captured as a free variable. This test
    -- has been simplified to avoid this issue by using only type variables.
    -- ============================================================

    hoistPolyCase "choose pattern from mutateTrace"
      -- Input simulates: let mutateTrace = ... in
      --   choose <~ (forLeft ~> forRight ~> e ~> either forLeft forRight e) $
      --   ... rest of mutateTrace ...
      -- The `choose` binding has type:
      --   forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
      -- (Simplified to avoid using "Either" as a type variable name)
      (mkLet [(nm "mutateTrace",
        lambda "mutate" (lambda "restore" (lambda "f"
          (Core.termLet $ mkLet [(nm "choose",
            -- choose = \forLeft -> \forRight -> \e -> forLeft e
            lambda "forLeft" (lambda "forRight" (lambda "e"
              (apply (var "forLeft") (var "e")))),
            -- Type: forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
            polyType ["t0", "t1", "t2"]
              (T.function
                (T.function (T.var "t0") (T.var "t1"))
                (T.function
                  (T.function (T.var "t2") (T.var "t1"))
                  (T.function (T.var "t0") (T.var "t1")))))]
            -- Body uses choose
            (apply (apply (apply (var "choose") (var "forLeft")) (var "forRight")) (var "e"))))),
        -- Full type of mutateTrace (using concrete types)
        monoType (T.function T.int32
          (T.function T.int32
            (T.function T.int32 T.int32))))]
        (var "mutateTrace"))
      -- Output: mutateTrace first (original), then mutateTrace_choose hoisted, MUST have t0, t1, t2 declared
      (mkLet [
        (nm "mutateTrace",
          lambda "mutate" (lambda "restore" (lambda "f"
            (apply (apply (apply (var "mutateTrace_choose") (var "forLeft")) (var "forRight")) (var "e")))),
          monoType (T.function T.int32
            (T.function T.int32
              (T.function T.int32 T.int32)))),
        (nm "mutateTrace_choose",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "forLeft" (lambda "forRight" (lambda "e"
            (apply (var "forLeft") (var "e"))))))),
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.function (T.var "t0") (T.var "t1"))
              (T.function
                (T.function (T.var "t2") (T.var "t1"))
                (T.function (T.var "t0") (T.var "t1")))))]
        (var "mutateTrace"))]
