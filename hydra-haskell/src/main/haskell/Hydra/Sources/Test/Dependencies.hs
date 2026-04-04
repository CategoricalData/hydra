{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for dependency analysis and let-term transformations
module Hydra.Sources.Test.Dependencies where

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

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Dependencies as DependenciesModule
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Dsl.Meta.Lib.Pairs as Pairs
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings


ns :: Namespace
ns = Namespace "hydra.test.dependencies"

module_ :: Module
module_ = Module ns elements
    [ShowCore.ns, DependenciesModule.ns, TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for dependency analysis and let-term transformations")
  where
    elements = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Local alias for polymorphic application (Phantoms.@@ applies TBindings; Terms.@@ only works on TTerm Term)
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- | Show a term as a string using ShowCore.term
showTerm :: TTerm Term -> TTerm String
showTerm t = ShowCore.term # t

-- | Helper for Term -> Term kernel function test cases
termCase :: String -> TTermDefinition (Term -> Term) -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
termCase cname func input output = universalCase cname (showTerm (func # input)) (showTerm output)

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper for single-binding let
letExpr :: String -> TTerm Term -> TTerm Term -> TTerm Term
letExpr varName value body = lets [(nm varName, value)] body

-- Helper for multi-binding let
multiLet :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
multiLet bindings body = lets ((\(n, v) -> (nm n, v)) <$> bindings) body

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- | Universal sortBindingsCase: applies topologicalSortBindingMap and shows result
sortBindingsCase :: String -> TTerm [(Name, Term)] -> TTerm [[(Name, Term)]] -> TTerm TestCaseWithMetadata
sortBindingsCase cname bindings expected = universalCase cname
  (showBindingGroups (DependenciesModule.topologicalSortBindingMap # Maps.fromList bindings))
  (showBindingGroups expected)
  where
    showBindingGroups :: TTerm [[(Name, Term)]] -> TTerm String
    showBindingGroups groups = ShowCore.list_ # showGroupFn # groups
    showGroupFn :: TTerm ([(Name, Term)] -> String)
    showGroupFn = Phantoms.lambda "group" $ ShowCore.list_ # showBindingFn # Phantoms.var "group"
    showBindingFn :: TTerm ((Name, Term) -> String)
    showBindingFn = Phantoms.lambda "pair" $ Strings.cat (Phantoms.list [
      Phantoms.string "(",
      Core.unName (Pairs.first (Phantoms.var "pair")),
      Phantoms.string ", ",
      ShowCore.term # Pairs.second (Phantoms.var "pair"),
      Phantoms.string ")"])

-- | Convenience helpers for specific kernel functions
flattenCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
flattenCase cname = termCase cname DependenciesModule.flattenLetTerms

liftLambdaCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
liftLambdaCase cname = termCase cname DependenciesModule.liftLambdaAboveLet

simplifyCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
simplifyCase cname = termCase cname DependenciesModule.simplifyTerm

-- | Test cases for term simplification (beta reduction)
simplifyTermGroup :: TTerm TestGroup
simplifyTermGroup = subgroup "simplifyTerm" [
    simplifyCase "const application with literal"
      (apply (lambda "x" (string "foo")) (int32 42))
      (string "foo"),

    simplifyCase "identity application"
      (apply
        (lambda "x" (list [var "x", var "x"]))
        (var "y"))
      (list [var "y", var "y"]),

    simplifyCase "unused parameter"
      (apply (lambda "x" (string "foo")) (var "y"))
      (string "foo"),

    simplifyCase "nested lambda applications"
      (apply
        (lambda "x"
          (apply
            (lambda "a" (list [string "foo", var "a"]))
            (var "x")))
        (var "y"))
      (list [string "foo", var "y"])]

-- | Test cases for flattening nested let terms
flattenLetTermsGroup :: TTerm TestGroup
flattenLetTermsGroup = subgroup "flattenLetTerms" [
    flattenCase "non-let term unchanged"
      (int32 42)
      (int32 42),

    flattenCase "list term unchanged"
      (list [string "foo"])
      (list [string "foo"]),

    flattenCase "sequential lets in body are flattened"
      -- let x = 1 in (let y = 2 in [x, y]) becomes let x = 1, y = 2 in [x, y]
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (list [var "x", var "y"])))
      (multiLet [("x", int32 1), ("y", int32 2)]
        (list [var "x", var "y"])),

    -- Nested bindings are flattened with prefix renaming
    -- Dependencies come BEFORE bindings that use them (important for hoisting)
    flattenCase "nested binding in let value is flattened"
      -- let a = 1; b = (let x = 1; y = 2 in [x, y]) in [a, b]
      (multiLet [
        ("a", int32 1),
        ("b", multiLet [("x", int32 1), ("y", int32 2)]
                (list [var "x", var "y"]))]
        (list [var "a", var "b"]))
      -- let a = 1; b_x = 1; b_y = 2; b = [b_x, b_y] in [a, b]
      -- Note: dependencies (b_x, b_y) come before b
      (multiLet [
        ("a", int32 1),
        ("b_x", int32 1),
        ("b_y", int32 2),
        ("b", list [var "b_x", var "b_y"])]
        (list [var "a", var "b"])),

    -- Multiple levels of nesting
    -- Dependencies come BEFORE bindings that use them (important for hoisting)
    flattenCase "multiple levels of nesting are flattened"
      -- let a = 1; b = (let x = 1; y = (let p = 137; q = [x, 5] in [a, q]) in [x, y]) in [a, b]
      (multiLet [
        ("a", int32 1),
        ("b", multiLet [
          ("x", int32 1),
          ("y", multiLet [
            ("p", int32 137),
            ("q", list [var "x", int32 5])]
            (list [var "a", var "q"]))]
          (list [var "x", var "y"]))]
        (list [var "a", var "b"]))
      -- Flattened with proper prefixes
      -- Order: a, then b's deps (b_x, b_y's deps (b_y_p, b_y_q), b_y), then b
      (multiLet [
        ("a", int32 1),
        ("b_x", int32 1),
        ("b_y_p", int32 137),
        ("b_y_q", list [var "b_x", int32 5]),
        ("b_y", list [var "a", var "b_y_q"]),
        ("b", list [var "b_x", var "b_y"])]
        (list [var "a", var "b"]))]

-- | Test cases for lifting lambda above let
liftLambdaAboveLetGroup :: TTerm TestGroup
liftLambdaAboveLetGroup = subgroup "liftLambdaAboveLet" [
    liftLambdaCase "simple let with lambda in body"
      (letExpr "x" (int32 42)
        (lambda "y" (var "x")))
      (lambda "y"
        (letExpr "x" (int32 42)
          (var "x"))),

    liftLambdaCase "bare lambda unchanged"
      (lambda "x" (var "x"))
      (lambda "x" (var "x")),

    liftLambdaCase "bare let unchanged"
      (letExpr "x" (int32 42) (var "x"))
      (letExpr "x" (int32 42) (var "x")),

    liftLambdaCase "lambda with let in body unchanged"
      (lambda "y" (letExpr "x" (int32 42) (var "x")))
      (lambda "y" (letExpr "x" (int32 42) (var "x"))),

    liftLambdaCase "let with two nested lambdas"
      (letExpr "x" (int32 42)
        (lambda "y" (lambda "z" (var "x"))))
      (lambda "y"
        (lambda "z"
          (letExpr "x" (int32 42) (var "x")))),

    liftLambdaCase "lambda inside let body already above let"
      (lambda "x"
        (lambda "y"
          (letExpr "z" (int32 42) (var "z"))))
      (lambda "x"
        (lambda "y"
          (letExpr "z" (int32 42) (var "z")))),

    -- Multiple bindings and nested lets
    liftLambdaCase "let without lambda in body unchanged"
      (multiLet [("x", int32 42), ("y", string "hello")]
        (pair (var "x") (var "y")))
      (multiLet [("x", int32 42), ("y", string "hello")]
        (pair (var "x") (var "y"))),

    liftLambdaCase "multiple let bindings with lambda"
      (multiLet [("x", int32 42), ("y", string "hello")]
        (lambda "z" (var "x")))
      (lambda "z"
        (multiLet [("x", int32 42), ("y", string "hello")]
          (var "x"))),

    liftLambdaCase "nested lets with lambda at innermost level"
      (letExpr "x" (int32 42)
        (letExpr "y" (string "hello")
          (lambda "z" (var "x"))))
      (lambda "z"
        (letExpr "x" (int32 42)
          (letExpr "y" (string "hello")
            (var "x")))),

    liftLambdaCase "lambda between two lets"
      (letExpr "x" (int32 42)
        (lambda "y"
          (letExpr "z" (string "hello")
            (var "x"))))
      (lambda "y"
        (letExpr "x" (int32 42)
          (letExpr "z" (string "hello")
            (var "x")))),

    liftLambdaCase "multiple lambdas between nested lets"
      (letExpr "a" (int32 1)
        (lambda "x"
          (lambda "y"
            (letExpr "b" (int32 2)
              (var "a")))))
      (lambda "x"
        (lambda "y"
          (letExpr "a" (int32 1)
            (letExpr "b" (int32 2)
              (var "a"))))),

    liftLambdaCase "multiple lambdas already above let"
      (lambda "x"
        (lambda "y"
          (letExpr "z" (int32 42)
            (var "z"))))
      (lambda "x"
        (lambda "y"
          (letExpr "z" (int32 42)
            (var "z")))),

    -- Annotation cases
    liftLambdaCase "annotation above let containing lambda"
      (annot emptyAnnMap
        (letExpr "x" (int32 42) (lambda "y" (var "x"))))
      (annot emptyAnnMap
        (lambda "y" (letExpr "x" (int32 42) (var "x")))),

    liftLambdaCase "annotation above lambda in let body"
      (letExpr "x" (int32 42)
        (annot emptyAnnMap (lambda "y" (var "x"))))
      (lambda "y"
        (annot emptyAnnMap (letExpr "x" (int32 42) (var "x")))),

    liftLambdaCase "annotation between two lambdas"
      (letExpr "x" (int32 42)
        (lambda "y"
          (annot emptyAnnMap (lambda "z" (var "x")))))
      (lambda "y"
        (lambda "z"
          (annot emptyAnnMap (letExpr "x" (int32 42) (var "x"))))),

    liftLambdaCase "annotation on the body of lambda in let"
      (letExpr "x" (int32 42)
        (lambda "y"
          (annot emptyAnnMap (var "x"))))
      (lambda "y"
        (letExpr "x" (int32 42)
          (annot emptyAnnMap (var "x")))),

    liftLambdaCase "annotation on lambda already above let"
      (annot emptyAnnMap
        (lambda "y" (letExpr "x" (int32 42) (var "x"))))
      (annot emptyAnnMap
        (lambda "y" (letExpr "x" (int32 42) (var "x")))),

    -- Recursive lifting in nested structures
    liftLambdaCase "let-lambda inside a list"
      (list [
        int32 1,
        letExpr "x" (int32 42) (lambda "y" (var "x")),
        int32 2])
      (list [
        int32 1,
        lambda "y" (letExpr "x" (int32 42) (var "x")),
        int32 2]),

    liftLambdaCase "let-lambda in multiple list elements"
      (list [
        letExpr "x" (int32 1) (lambda "y" (var "x")),
        letExpr "z" (int32 2) (lambda "w" (var "z"))])
      (list [
        lambda "y" (letExpr "x" (int32 1) (var "x")),
        lambda "w" (letExpr "z" (int32 2) (var "z"))]),

    liftLambdaCase "let-lambda in a let binding value"
      (letExpr "f" (letExpr "x" (int32 42) (lambda "y" (var "x")))
        (var "f"))
      (letExpr "f" (lambda "y" (letExpr "x" (int32 42) (var "x")))
        (var "f")),

    liftLambdaCase "let-lambda inside a pair"
      (pair
        (letExpr "x" (int32 42) (lambda "y" (var "x")))
        (string "test"))
      (pair
        (lambda "y" (letExpr "x" (int32 42) (var "x")))
        (string "test")),

    liftLambdaCase "let-lambda in both elements of a pair"
      (pair
        (letExpr "x" (int32 1) (lambda "y" (var "x")))
        (letExpr "z" (int32 2) (lambda "w" (var "z"))))
      (pair
        (lambda "y" (letExpr "x" (int32 1) (var "x")))
        (lambda "w" (letExpr "z" (int32 2) (var "z")))),

    liftLambdaCase "let-lambda inside lambda body"
      (lambda "outer"
        (letExpr "x" (int32 42) (lambda "inner" (var "x"))))
      (lambda "outer"
        (lambda "inner" (letExpr "x" (int32 42) (var "x"))))]

-- | Test cases for topological sort of bindings
-- The function topologicalSortBindingMap takes a map of (name -> term) bindings
-- and returns groups of bindings in topological order, where each group contains
-- mutually recursive bindings (strongly connected components).
topologicalSortBindingsGroup :: TTerm TestGroup
topologicalSortBindingsGroup = subgroup "topologicalSortBindings" [
    sortBindingsCase "isolated bindings"
      (Phantoms.list [
        Phantoms.pair (nm "a") (string "foo"),
        Phantoms.pair (nm "b") (string "bar")])
      (Phantoms.list [
        Phantoms.list [Phantoms.pair (nm "a") (string "foo")],
        Phantoms.list [Phantoms.pair (nm "b") (string "bar")]]),

    sortBindingsCase "single recursive binding"
      (Phantoms.list [
        Phantoms.pair (nm "a") (list [var "a"])])
      (Phantoms.list [
        Phantoms.list [Phantoms.pair (nm "a") (list [var "a"])]]),

    sortBindingsCase "mutually recursive bindings"
      (Phantoms.list [
        Phantoms.pair (nm "a") (list [var "b"]),
        Phantoms.pair (nm "b") (list [var "a"])])
      (Phantoms.list [
        Phantoms.list [
          Phantoms.pair (nm "a") (list [var "b"]),
          Phantoms.pair (nm "b") (list [var "a"])]]),

    sortBindingsCase "mixed bindings"
      (Phantoms.list [
        Phantoms.pair (nm "a") (var "b"),
        Phantoms.pair (nm "b") (list [var "a", var "c"]),
        Phantoms.pair (nm "c") (string "foo"),
        Phantoms.pair (nm "d") (string "bar")])
      (Phantoms.list [
        Phantoms.list [Phantoms.pair (nm "c") (string "foo")],
        Phantoms.list [
          Phantoms.pair (nm "a") (var "b"),
          Phantoms.pair (nm "b") (list [var "a", var "c"])],
        Phantoms.list [Phantoms.pair (nm "d") (string "bar")]])]

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for dependency analysis and let-term transformations" $
    supergroup "dependencies" [
      simplifyTermGroup,
      flattenLetTermsGroup,
      liftLambdaAboveLetGroup,
      topologicalSortBindingsGroup]
