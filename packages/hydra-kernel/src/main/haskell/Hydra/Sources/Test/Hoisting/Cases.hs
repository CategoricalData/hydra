{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for subterm hoisting and case statement hoisting transformations

module Hydra.Sources.Test.Hoisting.Cases where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing hiding (
  hoistPredicateNothing, hoistPredicateLists, hoistPredicateApplications, hoistPredicateCaseStatements)
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
import qualified Hydra.Dsl.Meta.Lib.Pairs as Pairs

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Hoisting as HoistingModule
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical


ns :: Namespace
ns = Namespace "hydra.test.hoisting.cases"

module_ :: Module
module_ = Module ns definitions
    [ShowCore.ns, HoistingModule.ns, Lexical.ns]
    kernelTypesNamespaces $
    Just "Test cases for subterm hoisting and case statement hoisting"
  where
    definitions = [Phantoms.toDefinition allTests]

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for subterm hoisting and case statement hoisting" $
    supergroup "hoistCases" [
      hoistSubtermsGroup,
      hoistCaseStatementsGroup]

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- Local alias for polymorphic application (Phantoms.@@ applies TBindings; Terms.@@ only works on TTerm Term)
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- | Show a term as a string using ShowCore.term
showTerm :: TTerm Term -> TTerm String
showTerm t = ShowCore.term # t

-- Field constructor for cases/match (Phantoms.>>: creates Field; unqualified >>: from Testing creates tuples)
(~>:) :: AsTerm t a => Name -> t -> Field
(~>:) = (Phantoms.>>:)
infixr 0 ~>:

-- | Predicate: never hoist anything
hoistPredicateNothing :: TTerm (([SubtermStep], Term) -> Bool)
hoistPredicateNothing = Phantoms.lambda "_" Phantoms.false

-- | Predicate: hoist list terms
hoistPredicateLists :: TTerm (([SubtermStep], Term) -> Bool)
hoistPredicateLists = Phantoms.lambda "pt" $
  Phantoms.cases _Term (Pairs.second (Phantoms.var "pt")) (Just Phantoms.false) [
    _Term_list ~>: Phantoms.lambda "_" Phantoms.true]

-- | Predicate: hoist function applications
hoistPredicateApplications :: TTerm (([SubtermStep], Term) -> Bool)
hoistPredicateApplications = Phantoms.lambda "pt" $
  Phantoms.cases _Term (Pairs.second (Phantoms.var "pt")) (Just Phantoms.false) [
    _Term_application ~>: Phantoms.lambda "_" Phantoms.true]

-- | Predicate: hoist case statements (elimination unions)
hoistPredicateCaseStatements :: TTerm (([SubtermStep], Term) -> Bool)
hoistPredicateCaseStatements = Phantoms.lambda "pt" $
  Phantoms.cases _Term (Pairs.second (Phantoms.var "pt")) (Just Phantoms.false) [
    _Term_cases ~>: Phantoms.lambda "_" Phantoms.true]

-- | Universal hoistSubterms test case
hoistCase :: String -> TTerm (([SubtermStep], Term) -> Bool) -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
hoistCase cname predicate input output = universalCase cname
  (showTerm (HoistingModule.hoistSubterms # predicate # Lexical.emptyGraph # input))
  (showTerm output)

-- | Local universal version of hoistCaseStatementsCase
hoistCaseStatementsCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
hoistCaseStatementsCase cname input output = universalCase cname
  (showTerm (HoistingModule.hoistCaseStatements # Lexical.emptyGraph # input))
  (showTerm output)

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
        (letExpr "_hoist_x_body_1" (list [int32 1, int32 2, int32 3])
          (apply (var "f") (var "_hoist_x_body_1")))),

    hoistCase "hoistLists: multiple lists in body are hoisted together"
      hoistPredicateLists
      -- Input: let x = 1 in pair [1, 2] [3, 4]
      (letExpr "x" (int32 1)
        (apply (apply (var "pair") (list [int32 1, int32 2]))
                                         (list [int32 3, int32 4])))
      -- Output: body is wrapped in local let with both hoisted lists
      (letExpr "x" (int32 1)
        (multiLet [
          ("_hoist_x_body_1", list [int32 1, int32 2]),
          ("_hoist_x_body_2", list [int32 3, int32 4])]
          (apply (apply (var "pair") (var "_hoist_x_body_1")) (var "_hoist_x_body_2")))),

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
          ("_hoist_x_body_1", list [int32 1, int32 2]),
          ("_hoist_x_body_2", list [var "_hoist_x_body_1", int32 3])]
          (apply (var "f") (var "_hoist_x_body_2")))),

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
        (letExpr "_hoist_x_body_1" (apply (var "f") (var "x"))
          (list [var "_hoist_x_body_1", var "y"]))),

    hoistCase "hoistApplications: application in record field is hoisted"
      hoistPredicateApplications
      -- Input: let x = 1 in {value: f x}
      (letExpr "x" (int32 1)
        (record (nm "Data") [(nm "value", apply (var "f") (var "x"))]))
      -- Output: body is wrapped in local let
      (letExpr "x" (int32 1)
        (letExpr "_hoist_x_body_1" (apply (var "f") (var "x"))
          (record (nm "Data") [(nm "value", var "_hoist_x_body_1")]))),

    hoistCase "hoistApplications: nested applications hoisted from inside out"
      hoistPredicateApplications
      -- Input: let x = 1 in [f (g x)]
      (letExpr "x" (int32 1)
        (list [apply (var "f") (apply (var "g") (var "x"))]))
      -- Output: inner application hoisted first, then outer
      (letExpr "x" (int32 1)
        (multiLet [
          ("_hoist_x_body_1", apply (var "g") (var "x")),
          ("_hoist_x_body_2", apply (var "f") (var "_hoist_x_body_1"))]
          (list [var "_hoist_x_body_2"]))),

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
        (letExpr "_hoist_x_body_1"
          (match (nm "Optional") (just $ var "x")
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (apply (var "f") (var "_hoist_x_body_1")))),

    hoistCase "hoistCaseStatements: case in list element is hoisted"
      hoistPredicateCaseStatements
      -- Input: let x = 1 in [match y with ok -> x | err -> 0]
      (letExpr "x" (int32 1)
        (list [match (nm "Result") (just $ var "y")
          [(nm "ok", var "x"),
           (nm "err", int32 0)]]))
      -- Output: body is wrapped in local let
      (letExpr "x" (int32 1)
        (letExpr "_hoist_x_body_1"
          (match (nm "Result") (just $ var "y")
            [(nm "ok", var "x"),
             (nm "err", int32 0)])
          (list [var "_hoist_x_body_1"]))),

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
          (letExpr "_hoist_y_body_1" (list [var "x", var "y"])
            (apply (var "f") (var "_hoist_y_body_1"))))),

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
        (letExpr "_hoist_x_body_1" (list [var "x", int32 2])
          (apply (var "f") (var "_hoist_x_body_1")))),

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
          (letExpr "_hoist_x_body_1" (list [var "y", var "x"])
            (apply (var "f") (var "_hoist_x_body_1"))))),

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
        (letExpr "_hoist_x_body_1" (list [var "x", int32 2])
          (lambda "y" (apply (var "f") (var "_hoist_x_body_1"))))),

    -- Case 4: Lambda-bound variable between let and hoisted term, IS free in hoisted term
    hoistCase "hoistLists: lambda-bound var free in hoisted term requires capture"
      hoistPredicateLists
      -- Input: let x = 1 in (\y -> f [x, y])
      -- y is lambda-bound between let and list, and y appears in the list [x, y]
      -- So [x, y] should be hoisted with y captured
      (letExpr "x" (int32 1)
        (lambda "y" (apply (var "f") (list [var "x", var "y"]))))
      -- Output: _hoist_x_body_1 = \y -> [x, y], reference becomes _hoist_x_body_1 y
      (letExpr "x" (int32 1)
        (letExpr "_hoist_x_body_1" (lambda "y" (list [var "x", var "y"]))
          (lambda "y" (apply (var "f") (apply (var "_hoist_x_body_1") (var "y")))))),

    -- Case 5: Multiple lambda-bound variables, only some free in hoisted term
    hoistCase "hoistLists: only free lambda-bound vars are captured"
      hoistPredicateLists
      -- Input: let x = 1 in (\a -> \b -> f [x, b])
      -- Both a and b are lambda-bound between let and list
      -- But only b appears in the list [x, b], so only b is captured
      (letExpr "x" (int32 1)
        (lambda "a" (lambda "b" (apply (var "f") (list [var "x", var "b"])))))
      -- Output: _hoist_x_body_1 = \b -> [x, b], reference becomes _hoist_x_body_1 b
      (letExpr "x" (int32 1)
        (letExpr "_hoist_x_body_1" (lambda "b" (list [var "x", var "b"]))
          (lambda "a" (lambda "b" (apply (var "f") (apply (var "_hoist_x_body_1") (var "b"))))))),

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
      -- Output: binding uses _hoist_x_1, body uses _hoist_x_body_1
      (letExpr "x"
        (letExpr "_hoist_x_1" (list [int32 1, int32 2])
          (apply (var "f") (var "_hoist_x_1")))
        (letExpr "_hoist_x_body_1" (list [int32 3, int32 4])
          (apply (var "g") (var "_hoist_x_body_1")))),

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
        (letExpr "_hoist_x_body_1"
          (match (nm "Optional") (just $ var "b")
            [(nm "just", lambda "w" (var "w")),
             (nm "nothing", int32 0)])
          (apply (var "f") (var "_hoist_x_body_1")))),

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
          (letExpr "_hoist_inner_body_1"
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (apply (var "g") (var "_hoist_inner_body_1"))))
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
        (var "f")),

    -- ============================================================
    -- Test: Case inside case default with let binding
    -- This is the pattern from isSimpleAssignment in Analysis:
    -- match term with
    --   specific_case -> ...
    --   _ -> let baseTerm = f(term) in match baseTerm with ...
    -- The inner case in the default branch's let body must be hoisted
    -- because Python can't encode match statements inline in case branches
    -- ============================================================

    hoistCaseStatementsCase "case in let body inside applied case default IS hoisted"
      -- Input: let f = (case x of just a -> a | _ -> let b = g(x) in (case b of ...)(b)) x
      -- The outer case is applied (through `cases`/`apply`), putting default branch
      -- at non-top-level. Inner applied case in default > let body must be hoisted.
      -- This is the pattern from isSimpleAssignment in Analysis.
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "a" (var "a")),
             (nm "nothing",
              letExpr "b" (apply (var "g") (var "x"))
                (apply
                  (match (nm "Result") nothing
                    [(nm "ok", lambda "y" (var "y")),
                     (nm "err", int32 0)])
                  (var "b")))])
          (var "x"))
        (var "f"))
      -- Output: inner applied case is hoisted within the inner let (preserving let-bound variable scoping)
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "a" (var "a")),
             (nm "nothing",
              letExpr "b" (apply (var "g") (var "x"))
                (letExpr "_hoist_b_body_1"
                  (match (nm "Result") nothing
                    [(nm "ok", lambda "y" (var "y")),
                     (nm "err", int32 0)])
                  (apply (var "_hoist_b_body_1") (var "b"))))])
          (var "x"))
        (var "f")),

    hoistCaseStatementsCase "case in let body inside applied case branch IS hoisted"
      -- Input: let f = (case x of just a -> let b = h(a) in (case b of ...)(b) | nothing -> 0) x
      -- Like above but inner case is in a named branch rather than default.
      -- 'a' IS lambda-bound (from the case branch), so it IS captured.
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "a"
              (letExpr "b" (apply (var "h") (var "a"))
                (apply
                  (match (nm "Result") nothing
                    [(nm "ok", lambda "y" (var "y")),
                     (nm "err", int32 0)])
                  (var "b")))),
             (nm "nothing", int32 0)])
          (var "x"))
        (var "f"))
      -- Output: inner case is hoisted within the inner let (preserving let-bound variable scoping)
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "a"
              (letExpr "b" (apply (var "h") (var "a"))
                (letExpr "_hoist_b_body_1"
                  (match (nm "Result") nothing
                    [(nm "ok", lambda "y" (var "y")),
                     (nm "err", int32 0)])
                  (apply (var "_hoist_b_body_1") (var "b"))))),
             (nm "nothing", int32 0)])
          (var "x"))
        (var "f")),

    -- ============================================================
    -- Test: Case applications (case function applied to argument)
    -- ============================================================

    hoistCaseStatementsCase "case application at top level of binding is NOT hoisted"
      -- Input: let f = (match Optional with ...) x in f
      -- The case application is at top level (one app LHS)
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (var "x"))
        (var "f"))
      -- Output: unchanged
      (letExpr "f"
        (apply
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (var "x"))
        (var "f")),

    hoistCaseStatementsCase "case application in arg position IS hoisted"
      -- Input: let f = g ((match Optional with ...) x) in f
      -- The bare case function is hoisted first (bottom-up), then the application
      -- uses the hoisted reference. 'x' is not lambda-bound so not captured.
      (letExpr "f"
        (apply (var "g")
          (apply
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (var "x")))
        (var "f"))
      -- Output: bare case function is hoisted, application becomes _hoist_f_1(x)
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (apply (var "g") (apply (var "_hoist_f_1") (var "x"))))
        (var "f")),

    hoistCaseStatementsCase "case application inside immediately-applied lambda IS hoisted"
      -- Input: let f = (\a -> (match Optional with ...) a) x in f
      -- The bare case function is inside \a -> (case ...) a. Bottom-up processing:
      -- The bare case function at path [LetBinding f, ApplicationFunction, LambdaBody, ApplicationFunction]
      -- gets hoisted. No captured variables (case function has no free vars).
      (letExpr "f"
        (apply
          (lambda "a"
            (apply
              (match (nm "Optional") nothing
                [(nm "just", lambda "y" (var "y")),
                 (nm "nothing", int32 0)])
              (var "a")))
          (var "x"))
        (var "f"))
      -- Output: bare case function is hoisted
      (letExpr "f"
        (letExpr "_hoist_f_1"
          (match (nm "Optional") nothing
            [(nm "just", lambda "y" (var "y")),
             (nm "nothing", int32 0)])
          (apply
            (lambda "a" (apply (var "_hoist_f_1") (var "a")))
            (var "x")))
        (var "f")),

    hoistCaseStatementsCase "case application in lambda body is NOT hoisted"
      -- Input: let f = \a -> (match Optional with ...) a in f
      -- Lambda body + one app LHS = still at top level
      (letExpr "f"
        (lambda "a"
          (apply
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (var "a")))
        (var "f"))
      -- Output: unchanged
      (letExpr "f"
        (lambda "a"
          (apply
            (match (nm "Optional") nothing
              [(nm "just", lambda "y" (var "y")),
               (nm "nothing", int32 0)])
            (var "a")))
        (var "f"))]
