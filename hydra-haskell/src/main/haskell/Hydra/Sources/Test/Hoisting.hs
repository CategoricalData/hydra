-- | Test cases for hoisting transformations
module Hydra.Sources.Test.Hoisting where

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
      (mkLet [(nm "x", T.int32 42, monoType MetaTypes.int32)] (T.var "x"))
      -- Output: unchanged
      (mkLet [(nm "x", T.int32 42, monoType MetaTypes.int32)] (T.var "x")),

    hoistPolyCase "no polymorphic bindings: multiple monomorphic bindings"
      -- Input: let x : Int32 = 1; y : String = "hi" in pair x y
      (mkLet [
        (nm "x", T.int32 1, monoType MetaTypes.int32),
        (nm "y", T.string "hi", monoType MetaTypes.string)]
        (T.apply (T.apply (T.var "pair") (T.var "x")) (T.var "y")))
      -- Output: unchanged
      (mkLet [
        (nm "x", T.int32 1, monoType MetaTypes.int32),
        (nm "y", T.string "hi", monoType MetaTypes.string)]
        (T.apply (T.apply (T.var "pair") (T.var "x")) (T.var "y"))),

    -- ============================================================
    -- Test: Single polymorphic binding at top level - stays in place
    -- ============================================================

    hoistPolyCase "single polymorphic binding: already at top level"
      -- Input: let id : forall a. a -> a = \x -> x in id 42
      (mkLet [(nm "id", T.lambda "x" (T.var "x"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
        (T.apply (T.var "id") (T.int32 42)))
      -- Output: unchanged (already at top)
      (mkLet [(nm "id", T.lambda "x" (T.var "x"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
        (T.apply (T.var "id") (T.int32 42))),

    -- ============================================================
    -- Test: Polymorphic binding in nested let - hoisted to top
    -- Note: disabled because we do not currently hoist from the top-level body
    -- ============================================================

--    hoistPolyCase "nested polymorphic binding: hoisted to top"
--      -- Input: let x : Int32 = 1 in (let id : forall a. a -> a = \y -> y in id x)
--      (mkLet [(nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (Core.termLet $ mkLet [(nm "id", T.lambda "y" (T.var "y"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
--          (T.apply (T.var "id") (T.var "x"))))
--      -- Output: let id : forall a. a -> a = \y -> y; x : Int32 = 1 in id x
--      -- (polymorphic binding hoisted, inner let removed since it's empty)
--      (mkLet [
--        (nm "id", T.lambda "y" (T.var "y"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
--        (nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (T.apply (T.var "id") (T.var "x"))),

    -- ============================================================
    -- Test: Mixed polymorphic and monomorphic bindings in nested let
    -- Note: disabled because we do not currently hoist from the top-level body
    -- ============================================================

--    hoistPolyCase "mixed bindings in nested let: only polymorphic hoisted"
--      -- Input: let x : Int32 = 1 in (let id : forall a. a -> a = \y -> y; z : Int32 = 2 in id z)
--      (mkLet [(nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (Core.termLet $ mkLet [
--          (nm "id", T.lambda "y" (T.var "y"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
--          (nm "z", T.int32 2, monoType MetaTypes.int32)]
--          (T.apply (T.var "id") (T.var "z"))))
--      -- Output: polymorphic id hoisted, monomorphic z stays in inner let
--      (mkLet [
--        (nm "id", T.lambda "y" (T.var "y"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
--        (nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (Core.termLet $ mkLet [
--          (nm "z", T.int32 2, monoType MetaTypes.int32)]
--          (T.apply (T.var "id") (T.var "z")))),

    -- ============================================================
    -- Test: Multiple polymorphic bindings in nested let
    -- Note: disabled because we do not currently hoist from the top-level body
    -- ============================================================

--    hoistPolyCase "multiple polymorphic bindings: all hoisted"
--      -- Input: let x : Int32 = 1 in (let id : forall a. a -> a = \y -> y; const : forall a b. a -> b -> a = \a -> \b -> a in const (id x) x)
--      (mkLet [(nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (Core.termLet $ mkLet [
--          (nm "id", T.lambda "y" (T.var "y"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
--          (nm "const", T.lambda "a" (T.lambda "b" (T.var "a")),
--            polyType ["a", "b"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "a"))))]
--          (T.apply (T.apply (T.var "const") (T.apply (T.var "id") (T.var "x"))) (T.var "x"))))
--      -- Output: both polymorphic bindings hoisted
--      (mkLet [
--        (nm "id", T.lambda "y" (T.var "y"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
--        (nm "const", T.lambda "a" (T.lambda "b" (T.var "a")),
--          polyType ["a", "b"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "a")))),
--        (nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (T.apply (T.apply (T.var "const") (T.apply (T.var "id") (T.var "x"))) (T.var "x"))),

    -- ============================================================
    -- Test: Polymorphic binding deeply nested
    -- Note: disabled because we do not currently hoist from the top-level body
    -- ============================================================

--    hoistPolyCase "deeply nested polymorphic binding: hoisted to top"
--      -- Input: let x = 1 in (let y = 2 in (let id : forall a. a -> a = \z -> z in id (x + y)))
--      (mkLet [(nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (Core.termLet $ mkLet [(nm "y", T.int32 2, monoType MetaTypes.int32)]
--          (Core.termLet $ mkLet [(nm "id", T.lambda "z" (T.var "z"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
--            (T.apply (T.var "id") (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))))))
--      -- Output: id hoisted to top, nested lets remain for monomorphic bindings
--      (mkLet [
--        (nm "id", T.lambda "z" (T.var "z"), polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
--        (nm "x", T.int32 1, monoType MetaTypes.int32)]
--        (Core.termLet $ mkLet [(nm "y", T.int32 2, monoType MetaTypes.int32)]
--          (T.apply (T.var "id") (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))))),

    -- ============================================================
    -- Test: Polymorphic binding inside lambda body
    -- ============================================================

    hoistPolyCase "polymorphic binding inside lambda: no capture"
      -- Input: let f = \a -> (let id : forall b. b -> b = \x -> x in id a) in f 42
      -- Here 'id' does not reference 'a', so it can be hoisted directly
      (mkLet [(nm "f",
        T.lambda "a" (Core.termLet $ mkLet [(nm "id", T.lambda "x" (T.var "x"), polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
          (T.apply (T.var "id") (T.var "a"))),
        monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32))]
        (T.apply (T.var "f") (T.int32 42)))
      -- Output: f comes first (original binding), then f_id is hoisted (no wrapping needed)
      (mkLet [
        (nm "f",
          T.lambda "a" (T.apply (T.var "f_id") (T.var "a")),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "f_id", T.tylam "b" (T.lambda "x" (T.var "x")), polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
        (T.apply (T.var "f") (T.int32 42))),

    -- ============================================================
    -- Test: Polymorphic binding captures lambda variable - requires wrapping
    -- ============================================================

    hoistPolyCase "polymorphic binding captures lambda variable: wrapped in lambda"
      -- Input: let f = \(a:String) -> (let g : forall b. b -> Pair<String, b> = \x -> pair a x in g 42) in f "hello"
      -- Here 'g' references 'a' (the enclosing lambda's variable), so when hoisted,
      -- it must be wrapped in a lambda that takes 'a', and the reference replaced with an application
      (mkLet [(nm "f",
        T.lambdaTyped "a" MetaTypes.string (Core.termLet $ mkLet [
          (nm "g", T.lambda "x" (T.apply (T.apply (T.var "pair") (T.var "a")) (T.var "x")),
            polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.pair MetaTypes.string (MetaTypes.var "b"))))]
          (T.apply (T.var "g") (T.int32 42))),
        monoType (MetaTypes.function MetaTypes.string (MetaTypes.pair MetaTypes.string MetaTypes.int32)))]
        (T.apply (T.var "f") (T.string "hello")))
      -- Output: f comes first (original binding), then f_g is hoisted wrapped in \(a:String) -> ..., and reference becomes (f_g a)
      (mkLet [
        (nm "f",
          T.lambdaTyped "a" MetaTypes.string (T.apply (T.apply (T.var "f_g") (T.var "a")) (T.int32 42)),
          monoType (MetaTypes.function MetaTypes.string (MetaTypes.pair MetaTypes.string MetaTypes.int32))),
        (nm "f_g", T.tylam "b" (T.lambdaTyped "a" MetaTypes.string (T.lambda "x" (T.apply (T.apply (T.var "pair") (T.var "a")) (T.var "x")))),
          polyType ["b"] (MetaTypes.function MetaTypes.string (MetaTypes.function (MetaTypes.var "b") (MetaTypes.pair MetaTypes.string (MetaTypes.var "b")))))]
        (T.apply (T.var "f") (T.string "hello"))),

    hoistPolyCase "polymorphic binding captures multiple lambda variables"
      -- Input: let f = \(a:Int32) -> \(b:Int32) -> (let g : forall c. c -> c = \x -> triple a b x in g 42) in f 1 2
      -- Here 'g' references both 'a' and 'b', so it needs to be wrapped with both
      (mkLet [(nm "f",
        T.lambdaTyped "a" MetaTypes.int32 (T.lambdaTyped "b" MetaTypes.int32 (Core.termLet $ mkLet [
          (nm "g", T.lambda "x" (T.apply (T.apply (T.apply (T.var "triple") (T.var "a")) (T.var "b")) (T.var "x")),
            polyType ["c"] (MetaTypes.function (MetaTypes.var "c") (MetaTypes.var "c")))]
          (T.apply (T.var "g") (T.int32 42)))),
        monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 MetaTypes.int32)))]
        (T.apply (T.apply (T.var "f") (T.int32 1)) (T.int32 2)))
      -- Output: f comes first (original binding), then f_g is wrapped in \(a:Int32) -> \(b:Int32) -> ..., references become ((f_g a) b)
      (mkLet [
        (nm "f",
          T.lambdaTyped "a" MetaTypes.int32 (T.lambdaTyped "b" MetaTypes.int32 (T.apply (T.apply (T.apply (T.var "f_g") (T.var "a")) (T.var "b")) (T.int32 42))),
          monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 MetaTypes.int32))),
        (nm "f_g", T.tylam "c" (T.lambdaTyped "a" MetaTypes.int32 (T.lambdaTyped "b" MetaTypes.int32 (T.lambda "x" (T.apply (T.apply (T.apply (T.var "triple") (T.var "a")) (T.var "b")) (T.var "x"))))),
          polyType ["c"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "c") (MetaTypes.var "c")))))]
        (T.apply (T.apply (T.var "f") (T.int32 1)) (T.int32 2))),

    hoistPolyCase "polymorphic binding captures some but not all lambda variables"
      -- Input: let f = \(a:Int32) -> \(b:Int32) -> (let g : forall c. c -> Pair<Int32, c> = \x -> pair a x in g b) in f 1 2
      -- Here 'g' only references 'a', not 'b'
      (mkLet [(nm "f",
        T.lambdaTyped "a" MetaTypes.int32 (T.lambdaTyped "b" MetaTypes.int32 (Core.termLet $ mkLet [
          (nm "g", T.lambda "x" (T.apply (T.apply (T.var "pair") (T.var "a")) (T.var "x")),
            polyType ["c"] (MetaTypes.function (MetaTypes.var "c") (MetaTypes.pair MetaTypes.int32 (MetaTypes.var "c"))))]
          (T.apply (T.var "g") (T.var "b")))),
        monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 (MetaTypes.pair MetaTypes.int32 MetaTypes.int32))))]
        (T.apply (T.apply (T.var "f") (T.int32 1)) (T.int32 2)))
      -- Output: f comes first (original binding), then f_g is wrapped only in \(a:Int32) -> ..., reference becomes (f_g a)
      (mkLet [
        (nm "f",
          T.lambdaTyped "a" MetaTypes.int32 (T.lambdaTyped "b" MetaTypes.int32 (T.apply (T.apply (T.var "f_g") (T.var "a")) (T.var "b"))),
          monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 (MetaTypes.pair MetaTypes.int32 MetaTypes.int32)))),
        (nm "f_g", T.tylam "c" (T.lambdaTyped "a" MetaTypes.int32 (T.lambda "x" (T.apply (T.apply (T.var "pair") (T.var "a")) (T.var "x")))),
          polyType ["c"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "c") (MetaTypes.pair MetaTypes.int32 (MetaTypes.var "c")))))]
        (T.apply (T.apply (T.var "f") (T.int32 1)) (T.int32 2))),

    -- ============================================================
    -- Test: Variable capture when hoisting polymorphic bindings
    -- When a polymorphic binding is hoisted OUT OF a lambda, variables
    -- from enclosing scopes (lambda parameters) must be captured.
    -- IMPORTANT: Sibling let bindings do NOT need to be captured because
    -- they remain in scope at the same level after hoisting.
    -- ============================================================

    -- NOTE: The following test cases were INCORRECT and have been commented out.
    -- They assumed that sibling let bindings need to be captured, but this is wrong.
    -- Sibling bindings in the same let block remain in scope after hoisting.
    --
    -- hoistPolyCase "polymorphic binding captures let-bound sibling: wrapped in lambda"
    --   -- WRONG: x and g are siblings at the same level. g doesn't need to capture x.
    --   -- Input: let x : Int32 = 42; g : forall a. a -> Pair<Int32, a> = \y -> pair x y in g "hello"
    --   (mkLet [
    --     (nm "x", T.int32 42, monoType MetaTypes.int32),
    --     (nm "g", T.lambda "y" (T.apply (T.apply (T.var "pair") (T.var "x")) (T.var "y")),
    --       polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.pair MetaTypes.int32 (MetaTypes.var "a"))))]
    --     (T.apply (T.var "g") (T.string "hello")))
    --   -- CORRECT output: g is not hoisted (no enclosing lambda to hoist out of), term unchanged
    --   (mkLet [
    --     (nm "x", T.int32 42, monoType MetaTypes.int32),
    --     (nm "g", T.lambda "y" (T.apply (T.apply (T.var "pair") (T.var "x")) (T.var "y")),
    --       polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.pair MetaTypes.int32 (MetaTypes.var "a"))))]
    --     (T.apply (T.var "g") (T.string "hello"))),
    --
    -- hoistPolyCase "polymorphic binding captures multiple let-bound siblings"
    --   -- WRONG: x, y, and g are all siblings at the same level. g doesn't need to capture x or y.
    --   -- Input: let x : Int32 = 1; y : Int32 = 2; g : forall a. a -> a = \z -> add (add x y) z in g 3
    --   (mkLet [
    --     (nm "x", T.int32 1, monoType MetaTypes.int32),
    --     (nm "y", T.int32 2, monoType MetaTypes.int32),
    --     (nm "g", T.lambda "z" (T.apply (T.apply (primitive _math_add)
    --               (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))) (T.var "z")),
    --       polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
    --     (T.apply (T.var "g") (T.int32 3)))
    --   -- CORRECT output: g is not hoisted (no enclosing lambda), term unchanged
    --   (mkLet [
    --     (nm "x", T.int32 1, monoType MetaTypes.int32),
    --     (nm "y", T.int32 2, monoType MetaTypes.int32),
    --     (nm "g", T.lambda "z" (T.apply (T.apply (primitive _math_add)
    --               (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))) (T.var "z")),
    --       polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
    --     (T.apply (T.var "g") (T.int32 3))),

    hoistPolyCase "polymorphic binding captures both lambda-bound and let-bound variables"
      -- Input: let f = \(a:Int32) -> (let x : Int32 = 1; g : forall b. b -> b = \y -> add (add a x) y in g 42) in f 10
      -- Here 'g' references 'a' (lambda-bound from enclosing scope) and 'x' (let-bound sibling).
      -- When g is hoisted to the top level, it goes OUTSIDE the inner let block, so it must capture
      -- both 'a' (from the enclosing lambda) AND 'x' (which is no longer a sibling after hoisting).
      (mkLet [(nm "f",
        T.lambdaTyped "a" MetaTypes.int32 (Core.termLet $ mkLet [
          (nm "x", T.int32 1, monoType MetaTypes.int32),
          (nm "g", T.lambda "y" (T.apply (T.apply (primitive _math_add)
                    (T.apply (T.apply (primitive _math_add) (T.var "a")) (T.var "x"))) (T.var "y")),
            polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
          (T.apply (T.var "g") (T.int32 42))),
        monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32))]
        (T.apply (T.var "f") (T.int32 10)))
      -- Output: f comes first (original binding), then f_g captures both 'a' and 'x', reference becomes ((f_g a) x)
      (mkLet [
        (nm "f",
          T.lambdaTyped "a" MetaTypes.int32 (Core.termLet $ mkLet [
            (nm "x", T.int32 1, monoType MetaTypes.int32)]
            (T.apply (T.apply (T.apply (T.var "f_g") (T.var "a")) (T.var "x")) (T.int32 42))),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "f_g", T.tylam "b" (T.lambdaTyped "a" MetaTypes.int32 (T.lambdaTyped "x" MetaTypes.int32
          (T.lambda "y" (T.apply (T.apply (primitive _math_add)
            (T.apply (T.apply (primitive _math_add) (T.var "a")) (T.var "x"))) (T.var "y"))))),
          polyType ["b"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))))]
        (T.apply (T.var "f") (T.int32 10))),

    -- NOTE: The following test case was INCORRECT and has been commented out.
    -- It assumed that sibling let bindings need to be captured transitively.
    --
    -- hoistPolyCase "transitive capture: polymorphic binding references hoisted polymorphic binding"
    --   -- WRONG: x, g, and h are all siblings at the same level. Neither g nor h needs to capture x.
    --   -- Input: let x : Int32 = 1; g : forall a. a -> a = \y -> add x y; h : forall b. b -> b = \z -> g z in h 42
    --   (mkLet [
    --     (nm "x", T.int32 1, monoType MetaTypes.int32),
    --     (nm "g", T.lambda "y" (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y")),
    --       polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
    --     (nm "h", T.lambda "z" (T.apply (T.var "g") (T.var "z")),
    --       polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
    --     (T.apply (T.var "h") (T.int32 42)))
    --   -- CORRECT output: No hoisting needed (no enclosing lambda), term unchanged
    --   (mkLet [
    --     (nm "x", T.int32 1, monoType MetaTypes.int32),
    --     (nm "g", T.lambda "y" (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y")),
    --       polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
    --     (nm "h", T.lambda "z" (T.apply (T.var "g") (T.var "z")),
    --       polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
    --     (T.apply (T.var "h") (T.int32 42))),

    hoistPolyCase "sibling polymorphic bindings inside lambda: one calls the other"
      -- Input: let wrapper = \outer : Int32 -> let g : forall a. a -> a = \y -> add outer y; h : forall b. b -> b = \z -> g z in h 42 in wrapper 10
      -- This mimics the structure in unifyTypeConstraints where bind and tryBinding are
      -- both inside withConstraint lambda and one calls the other.
      -- g captures 'outer', h calls g which means h must transitively capture 'outer'
      (mkLet [(nm "wrapper",
        T.lambdaTyped "outer" MetaTypes.int32 (Core.termLet $ mkLet [
          (nm "g", T.lambda "y" (T.apply (T.apply (primitive _math_add) (T.var "outer")) (T.var "y")),
            polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
          (nm "h", T.lambda "z" (T.apply (T.var "g") (T.var "z")),
            polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
          (T.apply (T.var "h") (T.int32 42))),
        monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32))]
        (T.apply (T.var "wrapper") (T.int32 10)))
      -- Output: wrapper first (original), then wrapper_g and wrapper_h hoisted (both capture outer)
      -- wrapper_g is wrapped: \outer -> \y -> add outer y
      -- wrapper_h is wrapped: \outer -> \z -> (wrapper_g outer) z
      -- The reference to wrapper_g inside wrapper_h becomes (wrapper_g outer)
      (mkLet [
        (nm "wrapper",
          T.lambdaTyped "outer" MetaTypes.int32 (T.apply (T.apply (T.var "wrapper_h") (T.var "outer")) (T.int32 42)),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "wrapper_g", T.tylam "a" (T.lambdaTyped "outer" MetaTypes.int32 (T.lambda "y" (T.apply (T.apply (primitive _math_add) (T.var "outer")) (T.var "y")))),
          polyType ["a"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))),
        (nm "wrapper_h", T.tylam "b" (T.lambdaTyped "outer" MetaTypes.int32 (T.lambda "z" (T.apply (T.apply (T.var "wrapper_g") (T.var "outer")) (T.var "z")))),
          polyType ["b"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b"))))]
        (T.apply (T.var "wrapper") (T.int32 10))),

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
        T.lambdaTyped "outer" MetaTypes.int32 (Core.termLet $ mkLet [
          (nm "g", T.lambda "v" (T.lambda "t" (T.apply (T.apply (primitive _math_add) (T.var "outer"))
            (T.apply (T.apply (primitive _math_add) (T.var "v")) (T.var "t")))),
            polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))),
          (nm "h", T.lambda "v" (T.lambda "t" (T.apply (T.apply (T.var "g") (T.var "v")) (T.var "t"))),
            polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b"))))]
          (T.apply (T.apply (T.var "h") (T.int32 1)) (T.int32 2))),
        monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32))]
        (T.apply (T.var "wrapper") (T.int32 10)))
      -- Output: wrapper first (original), then wrapper_g and wrapper_h hoisted (both capture outer)
      -- wrapper_g: \outer -> \v -> \t -> add outer (add v t)
      -- wrapper_h: \outer -> \v -> \t -> ((wrapper_g outer) v) t
      (mkLet [
        (nm "wrapper",
          T.lambdaTyped "outer" MetaTypes.int32 (T.apply (T.apply (T.apply (T.var "wrapper_h") (T.var "outer")) (T.int32 1)) (T.int32 2)),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "wrapper_g", T.tylam "a" (T.lambdaTyped "outer" MetaTypes.int32
          (T.lambda "v" (T.lambda "t" (T.apply (T.apply (primitive _math_add) (T.var "outer"))
            (T.apply (T.apply (primitive _math_add) (T.var "v")) (T.var "t")))))),
          polyType ["a"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "a") (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))))),
        (nm "wrapper_h", T.tylam "b" (T.lambdaTyped "outer" MetaTypes.int32
          (T.lambda "v" (T.lambda "t" (T.apply (T.apply (T.apply (T.var "wrapper_g") (T.var "outer")) (T.var "v")) (T.var "t"))))),
          polyType ["b"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "b") (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))))]
        (T.apply (T.var "wrapper") (T.int32 10))),

    -- ============================================================
    -- Test: Untyped bindings are treated as monomorphic
    -- ============================================================

    hoistPolyCase "untyped binding: not hoisted"
      -- Input: let x = 1 in (let y = 2 in x + y)
      -- Neither binding has a type, so neither is polymorphic
      (mkLetUntyped [(nm "x", T.int32 1)]
        (Core.termLet $ mkLetUntyped [(nm "y", T.int32 2)]
          (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))))
      -- Output: unchanged (untyped bindings are not polymorphic)
      (mkLetUntyped [(nm "x", T.int32 1)]
        (Core.termLet $ mkLetUntyped [(nm "y", T.int32 2)]
          (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y")))),

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
        (nm "id", T.lambda "x" (T.var "x"), monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "f",
          T.lambda "a" (Core.termLet $ mkLet [
            (nm "id2", T.lambda "y" (T.var "y"), polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
            (T.apply (T.var "id2") (T.apply (T.var "id") (T.var "a")))),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32))]
        (T.apply (T.var "f") (T.int32 42)))
      -- Output: id and f first (original), then f_id2 hoisted
      (mkLet [
        (nm "id", T.lambda "x" (T.var "x"), monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "f",
          T.lambda "a" (T.apply (T.var "f_id2") (T.apply (T.var "id") (T.var "a"))),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "f_id2", T.tylam "b" (T.lambda "y" (T.var "y")), polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
        (T.apply (T.var "f") (T.int32 42))),

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
        T.lambdaTyped "outer" MetaTypes.int32 (Core.termLet $ mkLet [
          -- g : forall a. a -> a (at outer let level)
          (nm "g",
            T.lambda "y" (T.var "y"),
            polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
          -- inner lambda
          (T.lambdaTyped "inner" MetaTypes.int32 (Core.termLet $ mkLet [
            -- h : forall b. b -> b (at inner let level, calls g)
            (nm "h",
              T.lambda "z" (T.apply (T.var "g") (T.var "z")),
              polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
            (T.apply (T.var "h") (T.int32 42))))),
        monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 MetaTypes.int32)))]
        (T.apply (T.apply (T.var "wrapper") (T.int32 10)) (T.int32 20)))
      -- Output: wrapper simplified, wrapper_g and wrapper_h hoisted.
      -- CRITICAL: wrapper_h references wrapper_g directly, NOT through a parameter.
      -- Neither g nor any enclosing lambda variables are captured by h,
      -- because g is polymorphic (excluded from capture) and h doesn't use outer/inner.
      (mkLet [
        (nm "wrapper",
          T.lambdaTyped "outer" MetaTypes.int32
            (T.lambdaTyped "inner" MetaTypes.int32
              (T.apply (T.var "wrapper_h") (T.int32 42))),
          monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 MetaTypes.int32))),
        -- wrapper_g: hoisted, no captures
        (nm "wrapper_g",
          T.tylam "a" (T.lambda "y" (T.var "y")),
          polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))),
        -- wrapper_h: hoisted, references wrapper_g directly (no g parameter)
        (nm "wrapper_h",
          T.tylam "b" (T.lambda "z" (T.apply (T.var "wrapper_g") (T.var "z"))),
          polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
        (T.apply (T.apply (T.var "wrapper") (T.int32 10)) (T.int32 20))),

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
        T.lambdaTyped "left" MetaTypes.int32
          (T.lambdaTyped "right" MetaTypes.int32
            (Core.termLet $ mkLet [
              (nm "sleft", T.apply (T.var "f") (T.var "left"), monoType MetaTypes.int32),
              (nm "sright", T.apply (T.var "f") (T.var "right"), monoType MetaTypes.int32),
              (nm "cannotUnify",
                T.lambda "x" (T.apply (T.apply (primitive _math_add) (T.var "sleft"))
                  (T.apply (T.apply (primitive _math_add) (T.var "sright")) (T.var "x"))),
                polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
              (T.apply (T.var "cannotUnify") (T.int32 42)))),
        monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 MetaTypes.int32)))]
        (T.apply (T.apply (T.var "wrapper") (T.int32 1)) (T.int32 2)))
      -- Expected output:
      -- wrapper is simplified, wrapper_cannotUnify is hoisted with sleft and sright captured
      -- wrapper_cannotUnify: forall a. \sleft:Int32 -> \sright:Int32 -> \x -> add sleft (add sright x)
      -- Reference becomes: ((wrapper_cannotUnify sleft) sright) 42
      (mkLet [
        (nm "wrapper",
          T.lambdaTyped "left" MetaTypes.int32
            (T.lambdaTyped "right" MetaTypes.int32
              (Core.termLet $ mkLet [
                (nm "sleft", T.apply (T.var "f") (T.var "left"), monoType MetaTypes.int32),
                (nm "sright", T.apply (T.var "f") (T.var "right"), monoType MetaTypes.int32)]
                (T.apply (T.apply (T.apply (T.var "wrapper_cannotUnify") (T.var "sleft")) (T.var "sright")) (T.int32 42)))),
          monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 MetaTypes.int32))),
        (nm "wrapper_cannotUnify",
          T.tylam "a"
            (T.lambdaTyped "sleft" MetaTypes.int32
              (T.lambdaTyped "sright" MetaTypes.int32
                (T.lambda "x" (T.apply (T.apply (primitive _math_add) (T.var "sleft"))
                  (T.apply (T.apply (primitive _math_add) (T.var "sright")) (T.var "x")))))),
          polyType ["a"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))))]
        (T.apply (T.apply (T.var "wrapper") (T.int32 1)) (T.int32 2))),

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
        T.lambdaTyped "left" MetaTypes.int32
          -- Outer nested let: sleft (mono)
          (Core.termLet $ mkLet [
            (nm "sleft", T.var "left", monoType MetaTypes.int32)]
            -- Middle nested let: cannotUnify (poly, refs sleft)
            (Core.termLet $ mkLet [
              (nm "cannotUnify",
                T.lambda "x" (T.apply (T.apply (primitive _math_add) (T.var "sleft")) (T.var "x")),
                polyType ["a"] (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))]
              -- Inner nested let: joinList (poly, refs cannotUnify)
              (Core.termLet $ mkLet [
                (nm "joinList",
                  T.lambda "y" (T.apply (T.var "cannotUnify") (T.var "y")),
                  polyType ["b"] (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b")))]
                (T.apply (T.var "joinList") (T.int32 42))))),
        monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32))]
        (T.apply (T.var "wrapper") (T.int32 1)))
      -- Expected: both cannotUnify and joinList are hoisted.
      -- wrapper_cannotUnify captures sleft.
      -- wrapper_joinList must ALSO capture sleft (transitively through cannotUnify).
      -- The reference to cannotUnify in joinList becomes wrapper_cannotUnify sleft,
      -- so joinList needs sleft in scope.
      (mkLet [
        (nm "wrapper",
          T.lambdaTyped "left" MetaTypes.int32
            (Core.termLet $ mkLet [
              (nm "sleft", T.var "left", monoType MetaTypes.int32)]
              (T.apply (T.apply (T.var "wrapper_joinList") (T.var "sleft")) (T.int32 42))),
          monoType (MetaTypes.function MetaTypes.int32 MetaTypes.int32)),
        (nm "wrapper_cannotUnify",
          T.tylam "a"
            (T.lambdaTyped "sleft" MetaTypes.int32
              (T.lambda "x" (T.apply (T.apply (primitive _math_add) (T.var "sleft")) (T.var "x")))),
          polyType ["a"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))),
        (nm "wrapper_joinList",
          T.tylam "b"
            (T.lambdaTyped "sleft" MetaTypes.int32
              (T.lambda "y" (T.apply (T.apply (T.var "wrapper_cannotUnify") (T.var "sleft")) (T.var "y")))),
          polyType ["b"] (MetaTypes.function MetaTypes.int32 (MetaTypes.function (MetaTypes.var "b") (MetaTypes.var "b"))))]
        (T.apply (T.var "wrapper") (T.int32 1)))]

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

--    -- Note: disabled because we do not currently hoist from the top-level body
--    hoistAllCase "simple nested let: inner binding hoisted"
--      -- Input: let x = 1 in (let y = 2 in x + y)
--      (mkLetUntyped [(nm "x", T.int32 1)]
--        (Core.termLet $ mkLetUntyped [(nm "y", T.int32 2)]
--          (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))))
--      -- Output: both bindings at top level (hoisted binding comes after original)
--      (mkLetUntyped [
--        (nm "x", T.int32 1),
--        (nm "y", T.int32 2)]
--        (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y"))),

    hoistAllCase "nested let inside lambda: binding hoisted with lambda capture"
      -- Input: let f = \a -> (let g = a + 1 in g * 2) in f 10
      (mkLetUntyped [(nm "f",
        T.lambda "a" (Core.termLet $ mkLetUntyped [(nm "g", T.apply (T.apply (primitive _math_add) (T.var "a")) (T.int32 1))]
          (T.apply (T.apply (primitive _math_mul) (T.var "g")) (T.int32 2))))]
        (T.apply (T.var "f") (T.int32 10)))
      -- Output: f comes first (original binding), then f_g is hoisted with lambda to capture 'a', reference becomes (f_g a)
      (mkLetUntyped [
        (nm "f",
          T.lambda "a" (T.apply (T.apply (primitive _math_mul) (T.apply (T.var "f_g") (T.var "a"))) (T.int32 2))),
        (nm "f_g", T.lambda "a" (T.apply (T.apply (primitive _math_add) (T.var "a")) (T.int32 1)))]
        (T.apply (T.var "f") (T.int32 10))),

    -- ============================================================
    -- Test: Type applications are processed normally (they don't introduce type variables)
    -- But the inner lambda still has nested let that gets hoisted
    -- ============================================================

    hoistAllCase "type application: nested let outside lambda CAN be hoisted"
      -- Input: let f = (let y = 1 in \x -> x + y) @Int32 in f 10
      -- The let is OUTSIDE the lambda, so y can be hoisted without capture
      (mkLetUntyped [(nm "f",
        T.tyapp (Core.termLet $ mkLetUntyped [(nm "y", T.int32 1)]
          (T.lambda "x" (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "y")))) MetaTypes.int32)]
        (T.apply (T.var "f") (T.int32 10)))
      -- Output: f comes first (original binding), then f_y is hoisted
      (mkLetUntyped [
        (nm "f",
          T.tyapp (T.lambda "x" (T.apply (T.apply (primitive _math_add) (T.var "x")) (T.var "f_y"))) MetaTypes.int32),
        (nm "f_y", T.int32 1)]
        (T.apply (T.var "f") (T.int32 10)))

    -- ============================================================
    -- Test: Mixed scenarios - type lambda inside regular lambda
    -- ============================================================

    -- Note: disabled because we do not currently hoist from the top-level body
--    hoistAllCase "mixed: let before type lambda can be hoisted"
--      -- Input: let outer = 1 in (let inner = Λt. \(x:t) -> x in inner @Int32 outer)
--      -- 'inner' is not inside a type lambda at the point where it's bound
--      (mkLetUntyped [(nm "outer", T.int32 1)]
--        (Core.termLet $ mkLetUntyped [(nm "inner", T.tylam "t" (T.lambdaTyped "x" (MetaTypes.var "t") (T.var "x")))]
--          (T.apply (T.tyapp (T.var "inner") MetaTypes.int32) (T.var "outer"))))
--      -- Output: inner is hoisted after outer (hoisted bindings come after original)
--      (mkLetUntyped [
--        (nm "outer", T.int32 1),
--        (nm "inner", T.tylam "t" (T.lambdaTyped "x" (MetaTypes.var "t") (T.var "x")))]
--        (T.apply (T.tyapp (T.var "inner") MetaTypes.int32) (T.var "outer")))
    ]

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
          T.lambda "forLeft" (T.lambda "forRight" (T.lambda "e"
            (T.apply (T.var "forLeft") (T.var "e")))),
          -- Type: forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function
              (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1"))
              (MetaTypes.function
                (MetaTypes.function (MetaTypes.var "t2") (MetaTypes.var "t1"))
                (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1")))))]
          (T.var "choose"),
        monoType (MetaTypes.function
          (MetaTypes.function MetaTypes.string MetaTypes.int32)
          (MetaTypes.function
            (MetaTypes.function MetaTypes.boolean MetaTypes.int32)
            (MetaTypes.function MetaTypes.string MetaTypes.int32))))]
        (T.var "f"))
      -- Output: f first (original), then f_choose hoisted with ALL type parameters preserved
      -- When generating Java, the method signature must declare t0, t1, t2
      (mkLet [
        (nm "f",
          T.var "f_choose",
          monoType (MetaTypes.function
            (MetaTypes.function MetaTypes.string MetaTypes.int32)
            (MetaTypes.function
              (MetaTypes.function MetaTypes.boolean MetaTypes.int32)
              (MetaTypes.function MetaTypes.string MetaTypes.int32)))),
        (nm "f_choose",
          T.tylam "t0" (T.tylam "t1" (T.tylam "t2" (T.lambda "forLeft" (T.lambda "forRight" (T.lambda "e"
            (T.apply (T.var "forLeft") (T.var "e"))))))),
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function
              (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1"))
              (MetaTypes.function
                (MetaTypes.function (MetaTypes.var "t2") (MetaTypes.var "t1"))
                (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1")))))]
        (T.var "f")),

    -- ============================================================
    -- Test: Type variable in return position only
    -- Type variables appearing only in the return type of a nested
    -- function must still be declared.
    -- ============================================================

    hoistPolyCase "type variable in return position only"
      -- Input: let f = (let returnT : forall t. () -> t = ... in returnT) in f
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "returnT",
          T.lambda "unit" (T.var "undefined"),
          polyType ["t"] (MetaTypes.function MetaTypes.unit (MetaTypes.var "t")))]
          (T.var "returnT"),
        monoType (MetaTypes.function MetaTypes.unit MetaTypes.int32))]
        (T.var "f"))
      -- Output: f first (original), then f_returnT hoisted with t declared
      (mkLet [
        (nm "f",
          T.var "f_returnT",
          monoType (MetaTypes.function MetaTypes.unit MetaTypes.int32)),
        (nm "f_returnT",
          T.tylam "t" (T.lambda "unit" (T.var "undefined")),
          polyType ["t"] (MetaTypes.function MetaTypes.unit (MetaTypes.var "t")))]
        (T.var "f")),

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
          T.lambda "x" (T.var "undefined"),
          -- Type: forall t0 t1 t2. ((t0, t1), t2) -> t0
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function
              (MetaTypes.pair (MetaTypes.pair (MetaTypes.var "t0") (MetaTypes.var "t1")) (MetaTypes.var "t2"))
              (MetaTypes.var "t0")))]
          (T.var "nested"),
        monoType (MetaTypes.function
          (MetaTypes.pair (MetaTypes.pair MetaTypes.string MetaTypes.int32) MetaTypes.boolean)
          MetaTypes.string))]
        (T.var "f"))
      -- Output: f first (original), then f_nested hoisted with all type parameters t0, t1, t2 declared
      (mkLet [
        (nm "f",
          T.var "f_nested",
          monoType (MetaTypes.function
            (MetaTypes.pair (MetaTypes.pair MetaTypes.string MetaTypes.int32) MetaTypes.boolean)
            MetaTypes.string)),
        (nm "f_nested",
          T.tylam "t0" (T.tylam "t1" (T.tylam "t2" (T.lambda "x" (T.var "undefined")))),
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function
              (MetaTypes.pair (MetaTypes.pair (MetaTypes.var "t0") (MetaTypes.var "t1")) (MetaTypes.var "t2"))
              (MetaTypes.var "t0")))]
        (T.var "f")),

    -- ============================================================
    -- Test: Multiple polymorphic bindings that share type variable names
    -- Each binding should independently track its own type variables.
    -- ============================================================

    hoistPolyCase "multiple bindings with overlapping type variable names"
      -- Input: let outer = (let id1 : forall t. t -> t = \x -> x; id2 : forall t. t -> t = \y -> y in pair id1 id2) in outer
      -- Both id1 and id2 use "t" but they are independent
      (mkLet [(nm "outer",
        Core.termLet $ mkLet [
          (nm "id1", T.lambda "x" (T.var "x"), polyType ["t"] (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "t"))),
          (nm "id2", T.lambda "y" (T.var "y"), polyType ["t"] (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "t")))]
          (T.apply (T.apply (T.var "pair") (T.var "id1")) (T.var "id2")),
        monoType (MetaTypes.pair
          (MetaTypes.function MetaTypes.int32 MetaTypes.int32)
          (MetaTypes.function MetaTypes.string MetaTypes.string)))]
        (T.var "outer"))
      -- Output: outer first (original), then outer_id1 and outer_id2 hoisted (each with their own t parameter)
      (mkLet [
        (nm "outer",
          T.apply (T.apply (T.var "pair") (T.var "outer_id1")) (T.var "outer_id2"),
          monoType (MetaTypes.pair
            (MetaTypes.function MetaTypes.int32 MetaTypes.int32)
            (MetaTypes.function MetaTypes.string MetaTypes.string))),
        (nm "outer_id1", T.tylam "t" (T.lambda "x" (T.var "x")), polyType ["t"] (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "t"))),
        (nm "outer_id2", T.tylam "t" (T.lambda "y" (T.var "y")), polyType ["t"] (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "t")))]
        (T.var "outer")),

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
        T.lambdaTyped "a" MetaTypes.string (Core.termLet $ mkLet [
          (nm "g",
            T.lambda "x" (T.apply (T.apply (T.var "pair") (T.var "a")) (T.var "x")),
            polyType ["t"] (MetaTypes.function (MetaTypes.var "t") (MetaTypes.pair MetaTypes.string (MetaTypes.var "t"))))]
          (T.apply (T.var "g") (T.int32 42))),
        monoType (MetaTypes.function MetaTypes.string (MetaTypes.pair MetaTypes.string MetaTypes.int32)))]
        (T.apply (T.var "f") (T.string "hello")))
      -- Output: f first (original), then f_g hoisted with lambda wrapper for 'a', and t must be declared
      -- Java signature: <t> Function<String, Function<t, Pair<String, t>>> f_g = a -> x -> pair(a, x);
      (mkLet [
        (nm "f",
          T.lambdaTyped "a" MetaTypes.string (T.apply (T.apply (T.var "f_g") (T.var "a")) (T.int32 42)),
          monoType (MetaTypes.function MetaTypes.string (MetaTypes.pair MetaTypes.string MetaTypes.int32))),
        (nm "f_g",
          T.tylam "t" (T.lambdaTyped "a" MetaTypes.string (T.lambda "x" (T.apply (T.apply (T.var "pair") (T.var "a")) (T.var "x")))),
          polyType ["t"] (MetaTypes.function MetaTypes.string (MetaTypes.function (MetaTypes.var "t") (MetaTypes.pair MetaTypes.string (MetaTypes.var "t")))))]
        (T.apply (T.var "f") (T.string "hello"))),

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
          T.lambda "s" (T.lambda "t" (T.var "undefined")),
          polyType ["s", "t", "v"]
            (MetaTypes.function (MetaTypes.var "s")
              (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "v"))))]
          (T.var "g"),
        monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.string MetaTypes.boolean)))]
        (T.var "f"))
      -- Output: f first (original), then f_g hoisted with s, t, v declared as type parameters
      (mkLet [
        (nm "f",
          T.var "f_g",
          monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.string MetaTypes.boolean))),
        (nm "f_g",
          T.tylam "s" (T.tylam "t" (T.tylam "v" (T.lambda "s" (T.lambda "t" (T.var "undefined"))))),
          polyType ["s", "t", "v"]
            (MetaTypes.function (MetaTypes.var "s")
              (MetaTypes.function (MetaTypes.var "t") (MetaTypes.var "v"))))]
        (T.var "f")),

    hoistPolyCase "numbered type variables like t0 t1 t2"
      -- Input: let f = (let g : forall t0 t1 t2. t0 -> t1 -> t2 = ... in g) in f
      -- t0, t1, t2 have length <= 4, common pattern in generated code
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "g",
          T.lambda "x" (T.lambda "y" (T.var "undefined")),
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function (MetaTypes.var "t0")
              (MetaTypes.function (MetaTypes.var "t1") (MetaTypes.var "t2"))))]
          (T.var "g"),
        monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.string MetaTypes.boolean)))]
        (T.var "f"))
      -- Output: f first (original), then f_g hoisted with t0, t1, t2 declared
      (mkLet [
        (nm "f",
          T.var "f_g",
          monoType (MetaTypes.function MetaTypes.int32 (MetaTypes.function MetaTypes.string MetaTypes.boolean))),
        (nm "f_g",
          T.tylam "t0" (T.tylam "t1" (T.tylam "t2" (T.lambda "x" (T.lambda "y" (T.var "undefined"))))),
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function (MetaTypes.var "t0")
              (MetaTypes.function (MetaTypes.var "t1") (MetaTypes.var "t2"))))]
        (T.var "f")),

    -- ============================================================
    -- Test: Complex "choose" pattern from mutateTrace
    -- This directly models the failing code in Monads.java
    -- NOTE: This test uses MetaTypes.var "Either" which causes "Either" to be
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
        T.lambda "mutate" (T.lambda "restore" (T.lambda "f"
          (Core.termLet $ mkLet [(nm "choose",
            -- choose = \forLeft -> \forRight -> \e -> forLeft e
            T.lambda "forLeft" (T.lambda "forRight" (T.lambda "e"
              (T.apply (T.var "forLeft") (T.var "e")))),
            -- Type: forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
            polyType ["t0", "t1", "t2"]
              (MetaTypes.function
                (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1"))
                (MetaTypes.function
                  (MetaTypes.function (MetaTypes.var "t2") (MetaTypes.var "t1"))
                  (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1")))))]
            -- Body uses choose
            (T.apply (T.apply (T.apply (T.var "choose") (T.var "forLeft")) (T.var "forRight")) (T.var "e"))))),
        -- Full type of mutateTrace (using concrete types)
        monoType (MetaTypes.function MetaTypes.int32
          (MetaTypes.function MetaTypes.int32
            (MetaTypes.function MetaTypes.int32 MetaTypes.int32))))]
        (T.var "mutateTrace"))
      -- Output: mutateTrace first (original), then mutateTrace_choose hoisted, MUST have t0, t1, t2 declared
      (mkLet [
        (nm "mutateTrace",
          T.lambda "mutate" (T.lambda "restore" (T.lambda "f"
            (T.apply (T.apply (T.apply (T.var "mutateTrace_choose") (T.var "forLeft")) (T.var "forRight")) (T.var "e")))),
          monoType (MetaTypes.function MetaTypes.int32
            (MetaTypes.function MetaTypes.int32
              (MetaTypes.function MetaTypes.int32 MetaTypes.int32)))),
        (nm "mutateTrace_choose",
          T.tylam "t0" (T.tylam "t1" (T.tylam "t2" (T.lambda "forLeft" (T.lambda "forRight" (T.lambda "e"
            (T.apply (T.var "forLeft") (T.var "e"))))))),
          polyType ["t0", "t1", "t2"]
            (MetaTypes.function
              (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1"))
              (MetaTypes.function
                (MetaTypes.function (MetaTypes.var "t2") (MetaTypes.var "t1"))
                (MetaTypes.function (MetaTypes.var "t0") (MetaTypes.var "t1")))))]
        (T.var "mutateTrace"))]

    -- NOTE: The following tests for "inner binding uses type variable from outer polymorphic context"
    -- and "monomorphic binding with type referencing outer type variable" have been removed.
    -- These test scenarios where a monomorphic binding inside a type lambda uses the outer type variable.
    -- The hoisting algorithm does NOT currently hoist bindings that are inside type lambdas (see hoistLetBindingsGroup
    -- tests which show type lambda is a BOUNDARY for hoisting). These tests were documenting expected future
    -- behavior that isn't implemented yet.

    -- ============================================================
    -- Test: Type inference introduces unquantified free type variables
    -- THIS IS A REGRESSION TEST for a bug where free type variables
    -- in a binding's type that are NOT from an enclosing type lambda
    -- and NOT already quantified in the type scheme were not captured.
    --
    -- Example: a binding has type scheme ∀a.(t2 -> t3 -> a -> a)
    -- Here 'a' is quantified, but 't2' and 't3' are FREE (not quantified).
    -- These free type variables must be added to the type scheme when hoisting.
    -- ============================================================

--    hoistPolyCase "free type variables from inference must be captured"
--      -- Input: let f = (let g : forall a. t2 -> t3 -> a -> a = \x -> \y -> \z -> z in g) in f
--      -- Here 'g' has type scheme with 'a' quantified, but 't2' and 't3' are free.
--      -- When hoisting, t2 and t3 must be added to the type scheme.
--      (mkLet [(nm "f",
--        Core.termLet $ mkLet [(nm "g",
--          T.lambda "x" (T.lambda "y" (T.lambda "z" (T.var "z"))),
--          -- Type scheme quantifies 'a', but uses free 't2' and 't3' (simulating inference result)
--          polyType ["a"]
--            (MetaTypes.function (MetaTypes.var "t2")
--              (MetaTypes.function (MetaTypes.var "t3")
--                (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))))]
--          (T.var "g"),
--        monoType (MetaTypes.function MetaTypes.int32
--          (MetaTypes.function MetaTypes.string
--            (MetaTypes.function MetaTypes.boolean MetaTypes.boolean))))]
--        (T.var "f"))
--      -- Output: f first (original), then g hoisted with t2, t3 added to type scheme (along with original 'a')
--      -- The order should be: outer type vars first, then inference-introduced vars, then original quantified vars
--      (mkLet [
--        (nm "f",
--          -- Reference needs type applications for t2 and t3
--          (T.tyapp (T.tyapp (T.var "g") (MetaTypes.var "t2")) (MetaTypes.var "t3")),
--          monoType (MetaTypes.function MetaTypes.int32
--            (MetaTypes.function MetaTypes.string
--              (MetaTypes.function MetaTypes.boolean MetaTypes.boolean)))),
--        (nm "g",
--          T.lambda "x" (T.lambda "y" (T.lambda "z" (T.var "z"))),
--          -- t2 and t3 are now quantified (added during hoisting), plus original 'a'
--          polyType ["t2", "t3", "a"]
--            (MetaTypes.function (MetaTypes.var "t2")
--              (MetaTypes.function (MetaTypes.var "t3")
--                (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a")))))]
--        (T.var "f")),

--    hoistPolyCase "mix of outer type lambda vars and inference-introduced free vars"
--      -- Input: let f = Λs. \x -> (let g : forall a. (t2 -> s) -> a -> a = \fn -> \y -> y in g) in f
--      -- Here 's' comes from outer type lambda, 't2' is free (from inference), 'a' is quantified.
--      -- When hoisting, both 's' and 't2' must be added to g's type scheme.
--      (mkLet [(nm "f",
--        T.tylam "s" (T.lambda "x"
--          (Core.termLet $ mkLet [(nm "g",
--            T.lambda "fn" (T.lambda "y" (T.var "y")),
--            -- 'a' quantified, 's' from outer type lambda, 't2' free from inference
--            polyType ["a"]
--              (MetaTypes.function
--                (MetaTypes.function (MetaTypes.var "t2") (MetaTypes.var "s"))
--                (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))))]
--            (T.var "g"))),
--        polyType ["s"]
--          (MetaTypes.function (MetaTypes.var "s") MetaTypes.int32))]
--        (T.var "f"))
--      -- Output: f first (original), then g hoisted with s (from outer), t2 (from inference), plus original 'a'
--      -- Order: outer type vars (s) first, then inference vars (t2), then original (a)
--      (mkLet [
--        (nm "f",
--          T.tylam "s" (T.lambda "x"
--            -- Reference needs type applications for s and t2
--            (T.tyapp (T.tyapp (T.var "g") (MetaTypes.var "s")) (MetaTypes.var "t2"))),
--          polyType ["s"]
--            (MetaTypes.function (MetaTypes.var "s") MetaTypes.int32)),
--        (nm "g",
--          T.lambda "fn" (T.lambda "y" (T.var "y")),
--          -- s, t2, and a are now all quantified
--          polyType ["s", "t2", "a"]
--            (MetaTypes.function
--              (MetaTypes.function (MetaTypes.var "t2") (MetaTypes.var "s"))
--              (MetaTypes.function (MetaTypes.var "a") (MetaTypes.var "a"))))]
--        (T.var "f"))]
