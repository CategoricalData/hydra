
-- | Test cases for term rewriting operations (free variables, simplify, flatten let, lift lambda)
module Hydra.Sources.Test.Rewriting where

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
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Data.Set                     as S

-- NOTE: This file previously used T for Terms and Ty for Types.
-- After standardization: Terms are unqualified, T is for Types.


ns :: Namespace
ns = Namespace "hydra.test.rewriting"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Test cases for term rewriting operations")
  where
    elements = [Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- | Test cases for rewriteAndFoldTermWithPath
-- These tests verify that the path-tracking rewrite function correctly tracks accessor paths
-- and properly folds values while rewriting terms
rewriteAndFoldTermWithPathGroup :: TTerm TestGroup
rewriteAndFoldTermWithPathGroup = subgroup "rewriteAndFoldTermWithPath" [
    -- The function is used by hoistSubtermsIntoLet, so we test that behavior
    -- Note: These test the path-tracking through the fold accumulator behavior

    -- Simple terms - no path-dependent rewriting needed
    foldOverTermCase "path tracking through application - sum literals"
      (apply (lambda "x" (var "x")) (int32 42))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 42),

    foldOverTermCase "path tracking through nested applications"
      (apply (apply (lambda "x" (lambda "y" (list [var "x", var "y"]))) (int32 1)) (int32 2))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 3),

    foldOverTermCase "path tracking through let bindings"
      (lets [(nm "x", int32 10)] (list [var "x", int32 32]))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 42),

    foldOverTermCase "path tracking through record fields"
      (record (nm "Point") [(nm "x", int32 10), (nm "y", int32 20)])
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 30),

    foldOverTermCase "path tracking through case branches"
      (match (nm "Result") nothing [(nm "ok", int32 1), (nm "err", int32 2)])
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 3),

    foldOverTermCase "path tracking through pair"
      (pair (int32 5) (int32 7))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 12),

    foldOverTermCase "path tracking through optional"
      (optional (just (int32 42)))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 42),

    foldOverTermCase "path tracking through wrapped term"
      (wrap (nm "Age") (int32 25))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 25),

    foldOverTermCase "path tracking through type lambda"
      (tylam "a" (int32 100))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 100),

    foldOverTermCase "path tracking through type application"
      (tyapp (int32 50) T.string)
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 50),

    foldOverTermCase "path tracking through set elements"
      (set [int32 1, int32 2, int32 3])
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 6),

    foldOverTermCase "deep nesting - application in lambda in let"
      (lets [(nm "f", lambda "x" (apply (var "x") (int32 5)))] (int32 10))
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 15),

    -- Collect list lengths tests verify proper path traversal
    foldOverTermCase "collect list lengths in nested structure"
      (list [list [int32 1, int32 2], list [int32 3]])
      Coders.traversalOrderPre
      foldOpCollectListLengths
      (list [int32 2, int32 2, int32 1]),

    foldOverTermCase "collect list lengths in let body"
      (lets [(nm "xs", list [int32 1])] (list [int32 2, int32 3]))
      Coders.traversalOrderPre
      foldOpCollectListLengths
      (list [int32 2, int32 1])]

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for term rewriting operations" $
    supergroup "rewriting" [
      freeVariablesGroup,
      simplifyTermGroup,
      flattenLetTermsGroup,
      liftLambdaAboveLetGroup,
      deannotateTermGroup,
      deannotateTypeGroup,
      topologicalSortBindingsGroup,
      normalizeTypeVariablesGroup,
      etaExpandTermGroup,
      foldOverTermGroup,
      rewriteTypeGroup,
      rewriteTermGroup,
      rewriteAndFoldTermWithPathGroup]

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper to build a set of Names
nameSet :: [String] -> TTerm (S.Set Name)
nameSet names = Phantoms.set $ (nm <$> names)

-- Helper for single-binding let
letExpr :: String -> TTerm Term -> TTerm Term -> TTerm Term
letExpr varName value body = lets [(nm varName, value)] body

-- Helper for multi-binding let
multiLet :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
multiLet bindings body = lets ((\(n, v) -> (nm n, v)) <$> bindings) body

-- | Test cases for free variables computation
freeVariablesGroup :: TTerm TestGroup
freeVariablesGroup = subgroup "freeVariables" [
    freeVarsCase "string literal has no free variables"
      (string "foo")
      (nameSet []),

    freeVarsCase "single variable"
      (var "x")
      (nameSet ["x"]),

    freeVarsCase "bound variable is not free"
      (lambda "y" (var "y"))
      (nameSet []),

    freeVarsCase "unbound variable in lambda body"
      (lambda "y" (var "x"))
      (nameSet ["x"]),

    freeVarsCase "mixed free and bound variables"
      (list [
        var "x",
        apply (lambda "y" (var "y")) (int32 42)])
      (nameSet ["x"]),

    freeVarsCase "multiple free variables"
      (list [
        var "x",
        apply (lambda "y" (var "y")) (var "y")])
      (nameSet ["x", "y"])]

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

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- | Test cases for deannotating terms (stripping top-level annotations)
-- Note: deannotateTerm only strips annotations at the top level, not recursively
deannotateTermGroup :: TTerm TestGroup
deannotateTermGroup = subgroup "deannotateTerm" [
    deannotateTermCase "unannotated literal unchanged"
      (int32 42)
      (int32 42),

    deannotateTermCase "unannotated variable unchanged"
      (var "x")
      (var "x"),

    deannotateTermCase "unannotated lambda unchanged"
      (lambda "x" (var "x"))
      (lambda "x" (var "x")),

    deannotateTermCase "single annotation stripped"
      (annot emptyAnnMap (int32 42))
      (int32 42),

    deannotateTermCase "nested annotations stripped"
      (annot emptyAnnMap (annot emptyAnnMap (int32 42)))
      (int32 42),

    deannotateTermCase "annotated lambda stripped"
      (annot emptyAnnMap (lambda "x" (var "x")))
      (lambda "x" (var "x")),

    deannotateTermCase "annotated application stripped"
      (annot emptyAnnMap (apply (var "f") (var "x")))
      (apply (var "f") (var "x"))]

-- | Test cases for deannotating types (stripping top-level annotations)
-- Note: deannotateType only strips annotations at the top level, not recursively
deannotateTypeGroup :: TTerm TestGroup
deannotateTypeGroup = subgroup "deannotateType" [
    deannotateTypeCase "unannotated primitive type unchanged"
      T.int32
      T.int32,

    deannotateTypeCase "unannotated string type unchanged"
      T.string
      T.string,

    deannotateTypeCase "unannotated function type unchanged"
      (T.function T.int32 T.string)
      (T.function T.int32 T.string),

    deannotateTypeCase "single annotation stripped"
      (T.annot emptyAnnMap T.int32)
      T.int32,

    deannotateTypeCase "nested annotations stripped"
      (T.annot emptyAnnMap (T.annot emptyAnnMap T.string))
      T.string,

    deannotateTypeCase "annotated list type stripped"
      (T.annot emptyAnnMap (T.list T.int32))
      (T.list T.int32),

    deannotateTypeCase "annotated function type stripped"
      (T.annot emptyAnnMap (T.function T.int32 T.string))
      (T.function T.int32 T.string)]

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

-- | Test cases for normalizing type variables in terms
-- The function normalizeTypeVariablesInTerm renames type variables to a canonical form (t0, t1, t2, etc.)
normalizeTypeVariablesGroup :: TTerm TestGroup
normalizeTypeVariablesGroup = subgroup "normalizeTypeVariables" [
    -- No type variables - terms should remain unchanged
    normalizeTypeVarsCase "literal without type variables unchanged"
      (int32 42)
      (int32 42),

    normalizeTypeVarsCase "simple let without type annotations unchanged"
      (letExpr "foo" (string "foo") (int32 42))
      (letExpr "foo" (string "foo") (int32 42)),

    normalizeTypeVarsCase "let with monomorphic type scheme unchanged"
      (letsTyped [("foo", string "foo", T.mono T.string)] (int32 42))
      (letsTyped [("foo", string "foo", T.mono T.string)] (int32 42)),

    normalizeTypeVarsCase "let with monomorphic binding referencing string"
      (letsTyped [("foo", string "foo", T.mono T.string)] (int32 42))
      (letsTyped [("foo", string "foo", T.mono T.string)] (int32 42)),

    -- Only free type variables - no normalization needed
    normalizeTypeVarsCase "polymorphic binding with free type variable unchanged"
      (letsTyped [("foo", var "bar", T.mono (T.var "a"))] (int32 42))
      (letsTyped [("foo", var "bar", T.mono (T.var "a"))] (int32 42)),

    normalizeTypeVarsCase "monomorphic binding with typed lambda unchanged"
      (letsTyped [("foo", string "foo", T.mono T.string)]
        (lambdaTyped "x" (T.function (T.var "a") T.int32) (int32 42)))
      (letsTyped [("foo", string "foo", T.mono T.string)]
        (lambdaTyped "x" (T.function (T.var "a") T.int32) (int32 42))),

    normalizeTypeVarsCase "polymorphic binding with typed lambda in body unchanged"
      (letsTyped [("foo", var "bar", T.mono (T.var "a"))]
        (lambdaTyped "x" (T.function (T.var "a") T.int32) (int32 42)))
      (letsTyped [("foo", var "bar", T.mono (T.var "a"))]
        (lambdaTyped "x" (T.function (T.var "a") T.int32) (int32 42))),

    -- Polymorphic let bindings should have type variables normalized
    normalizeTypeVarsCase "polymorphic identity function normalized"
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42)))
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
        (apply (var "id") (int32 42))),

    normalizeTypeVarsCase "polymorphic const function normalized"
      (letsTyped [("const", lambda "x" (lambda "y" (var "x")),
        T.poly ["a", "b"] (T.function (T.var "a") (T.function (T.var "b") (T.var "a"))))]
        (apply (apply (var "const") (int32 42)) (string "foo")))
      (letsTyped [("const", lambda "x" (lambda "y" (var "x")),
        T.poly ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") (T.var "t0"))))]
        (apply (apply (var "const") (int32 42)) (string "foo"))),

    -- Rewriting of bindings does not affect body (free variable in body coincides with bound variable in binding)
    normalizeTypeVarsCase "binding rewriting does not affect body with typed lambda"
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
        (lambdaTyped "x" (T.function (T.var "a") T.int32) (int32 42)))
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
        (lambdaTyped "x" (T.function (T.var "a") T.int32) (int32 42))),

    -- Nested polymorphic let bindings - each type scheme is normalized independently
    normalizeTypeVarsCase "nested polymorphic lets normalized"
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
        (letsTyped [("id2", lambda "y" (var "y"),
          T.poly ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "id") (apply (var "id2") (int32 42)))))
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
        (letsTyped [("id2", lambda "y" (var "y"),
          T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
          (apply (var "id") (apply (var "id2") (int32 42))))),

    normalizeTypeVarsCase "nested same substitution in bindings and environment"
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
        (letsTyped [("id2", lambda "x" (var "x"),
          T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
          (apply (var "id") (int32 42))))
      (letsTyped [("id", lambda "x" (var "x"),
        T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
        (letsTyped [("id2", lambda "x" (var "x"),
          T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
          (apply (var "id") (int32 42)))),

    -- Parent variable shadows child variable in nested lets with typed lambdas
    normalizeTypeVarsCase "parent type variable shadows child variable"
      (letsTyped [("id",
        letsTyped [("id2",
          lambdaTyped "x" (T.var "a") (var "x"),
          T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
          (lambdaTyped "y" (T.var "a") (apply (var "id2") (var "y"))),
        T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42)))
      (letsTyped [("id",
        letsTyped [("id2",
          lambdaTyped "x" (T.var "t1") (var "x"),
          T.poly ["t1"] (T.function (T.var "t1") (T.var "t1")))]
          (lambdaTyped "y" (T.var "t0") (apply (var "id2") (var "y"))),
        T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
        (apply (var "id") (int32 42))),

    -- No shadowing: distinct type variable names in nested lets
    normalizeTypeVarsCase "no shadowing distinct type variables"
      (letsTyped [("id",
        letsTyped [("id2",
          lambdaTyped "x" (T.var "b") (var "x"),
          T.poly ["b"] (T.function (T.var "b") (T.var "b")))]
          (lambdaTyped "y" (T.var "a") (apply (var "id2") (var "y"))),
        T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42)))
      (letsTyped [("id",
        letsTyped [("id2",
          lambdaTyped "x" (T.var "t1") (var "x"),
          T.poly ["t1"] (T.function (T.var "t1") (T.var "t1")))]
          (lambdaTyped "y" (T.var "t0") (apply (var "id2") (var "y"))),
        T.poly ["t0"] (T.function (T.var "t0") (T.var "t0")))]
        (apply (var "id") (int32 42))),

    -- Complex: locally free type variable in nested binding
    normalizeTypeVarsCase "locally free type variable in nested binding"
      (letsTyped [("fun1",
        lambdaTyped "x" (T.var "a") (lambdaTyped "y" (T.var "b")
          (letsTyped [("fun2",
            lambdaTyped "z" (T.var "c") (pair (var "z") (var "y")),
            T.poly ["c"] (T.function (T.var "c") (T.pair (T.var "c") (T.var "b"))))]
            (apply (var "fun2") (var "x")))),
        T.poly ["a", "b"] (T.function (T.var "a") (T.function (T.var "b") (T.pair (T.var "a") (T.var "b")))))]
        (apply (apply (var "fun1") (string "foo")) (int32 42)))
      (letsTyped [("fun1",
        lambdaTyped "x" (T.var "t0") (lambdaTyped "y" (T.var "t1")
          (letsTyped [("fun2",
            lambdaTyped "z" (T.var "t2") (pair (var "z") (var "y")),
            T.poly ["t2"] (T.function (T.var "t2") (T.pair (T.var "t2") (T.var "t1"))))]
            (apply (var "fun2") (var "x")))),
        T.poly ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") (T.pair (T.var "t0") (T.var "t1")))))]
        (apply (apply (var "fun1") (string "foo")) (int32 42)))]

-- | Test cases for eta expansion of terms
-- Eta expansion adds explicit lambda wrappers to partially applied functions
etaExpandTermGroup :: TTerm TestGroup
etaExpandTermGroup = subgroup "etaExpandTerm" [
    -- Terms that don't expand (already saturated or not functions)
    etaCase "integer literal unchanged"
      (int32 42)
      (int32 42),

    etaCase "string list unchanged"
      (list [string "foo", string "bar"])
      (list [string "foo", string "bar"]),

    etaCase "fully applied binary function unchanged"
      (apply (apply (primitive _strings_splitOn) (string "foo")) (string "bar"))
      (apply (apply (primitive _strings_splitOn) (string "foo")) (string "bar")),

    -- Lambda with fully applied primitive using a string literal (matches EtaExpansion.hs pattern)
    etaCase "lambda with fully applied primitive unchanged"
      (lambda "x" (apply (apply (primitive _strings_splitOn) (string ",")) (var "x")))
      (lambda "x" (apply (apply (primitive _strings_splitOn) (string ",")) (var "x"))),

    etaCase "lambda returning constant unchanged"
      (lambda "x" (int32 42))
      (lambda "x" (int32 42)),

    -- Bare primitives are NOT expanded (they stay as-is)
    etaCase "bare unary primitive unchanged"
      (primitive _strings_toLower)
      (primitive _strings_toLower),

    etaCase "bare binary primitive unchanged"
      (primitive _strings_splitOn)
      (primitive _strings_splitOn),

    etaCase "partially applied binary primitive expands to one lambda"
      (apply (primitive _strings_splitOn) (var "foo"))
      (lambda "v1" (apply (apply (primitive _strings_splitOn) (var "foo")) (var "v1"))),

    etaCase "projection expands to lambda"
      (project (nm "Person") (nm "firstName"))
      (lambda "v1" (apply (project (nm "Person") (nm "firstName")) (var "v1"))),

    -- Subterms within applications
    etaCase "partial application inside lambda expands"
      (lambda "x" (apply (primitive _strings_splitOn) (var "x")))
      (lambda "x" (lambda "v1" (apply (apply (primitive _strings_splitOn) (var "x")) (var "v1")))),

    -- Let bindings
    etaCase "let with constant body unchanged"
      (letExpr "foo" (int32 137) (int32 42))
      (letExpr "foo" (int32 137) (int32 42)),

    etaCase "let with bare primitive value unchanged"
      (letExpr "foo" (primitive _strings_splitOn) (var "foo"))
      (letExpr "foo" (primitive _strings_splitOn) (var "foo")),

    -- Complete applications are no-ops
    etaCase "fully applied unary unchanged"
      (apply (primitive _strings_toLower) (string "FOO"))
      (apply (primitive _strings_toLower) (string "FOO")),

    -- Subterms
    etaCase "partial application in list expands"
      (list [lambda "x" (list [string "foo"]), apply (primitive _strings_splitOn) (string "bar")])
      (list [lambda "x" (list [string "foo"]), lambda "v1" (apply (apply (primitive _strings_splitOn) (string "bar")) (var "v1"))])]

-- Helper to create labeled node (pair of label and list of children)
labeledNode :: String -> [TTerm Term] -> TTerm Term
labeledNode label children = pair (string label) (list children)

-- | Test cases for foldOverTerm
-- Using predefined fold operations: collectLabels, sumInt32Literals, collectListLengths
foldOverTermGroup :: TTerm TestGroup
foldOverTermGroup = subgroup "foldOverTerm" [
    -- collectLabels tests (from checkFoldOverTerm in RewritingSpec.hs)
    -- Nodes are represented as pairs: (label, children)
    foldOverTermCase "collect labels from single node - pre-order"
      (labeledNode "a" [])
      Coders.traversalOrderPre
      foldOpCollectLabels
      (list [string "a"]),

    foldOverTermCase "collect labels from tree - pre-order"
      (labeledNode "a" [labeledNode "b" [], labeledNode "c" [labeledNode "d" []]])
      Coders.traversalOrderPre
      foldOpCollectLabels
      (list [string "a", string "b", string "c", string "d"]),

    foldOverTermCase "collect labels from single node - post-order"
      (labeledNode "a" [])
      Coders.traversalOrderPost
      foldOpCollectLabels
      (list [string "a"]),

    foldOverTermCase "collect labels from tree - post-order"
      (labeledNode "a" [labeledNode "b" [], labeledNode "c" [labeledNode "d" []]])
      Coders.traversalOrderPost
      foldOpCollectLabels
      (list [string "b", string "d", string "c", string "a"]),

    -- sumInt32Literals tests (from testFoldOverTerm in RewritingSpec.hs)
    foldOverTermCase "sum int32 literals"
      (list [int32 42, apply (lambda "x" (var "x")) (int32 10)])
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (int32 52),

    -- collectListLengths tests (from testFoldOverTerm in RewritingSpec.hs)
    foldOverTermCase "collect list lengths - pre-order"
      (list [list [string "foo", string "bar"], apply (lambda "x" (var "x")) (list [string "quux"])])
      Coders.traversalOrderPre
      foldOpCollectListLengths
      (list [int32 2, int32 2, int32 1]),

    foldOverTermCase "collect list lengths - post-order"
      (list [list [string "foo", string "bar"], apply (lambda "x" (var "x")) (list [string "quux"])])
      Coders.traversalOrderPost
      foldOpCollectListLengths
      (list [int32 2, int32 1, int32 2])]

-- | Test cases for rewriteType
-- Using predefined type rewriter: replaceStringWithInt32
rewriteTypeGroup :: TTerm TestGroup
rewriteTypeGroup = subgroup "rewriteType" [
    rewriteTypeCase "String type in left side of either is replaced"
      (T.either_ T.string T.int32)
      (T.either_ T.int32 T.int32),

    rewriteTypeCase "String type in right side of either is replaced"
      (T.either_ T.int32 T.string)
      (T.either_ T.int32 T.int32),

    rewriteTypeCase "String types in both sides of either are replaced"
      (T.either_ T.string T.string)
      (T.either_ T.int32 T.int32),

    rewriteTypeCase "String type in nested either (left of left) is replaced"
      (T.either_ (T.either_ T.string T.int32) T.int64)
      (T.either_ (T.either_ T.int32 T.int32) T.int64),

    rewriteTypeCase "String type in nested either (right of right) is replaced"
      (T.either_ T.int64 (T.either_ T.int32 T.string))
      (T.either_ T.int64 (T.either_ T.int32 T.int32)),

    rewriteTypeCase "String types in complex nested either are all replaced"
      (T.either_ (T.either_ T.string T.string) (T.either_ T.string T.int64))
      (T.either_ (T.either_ T.int32 T.int32) (T.either_ T.int32 T.int64)),

    rewriteTypeCase "String in list type is replaced"
      (T.list T.string)
      (T.list T.int32),

    rewriteTypeCase "String in function domain is replaced"
      (T.function T.string T.int64)
      (T.function T.int32 T.int64),

    rewriteTypeCase "String in function codomain is replaced"
      (T.function T.int64 T.string)
      (T.function T.int64 T.int32),

    rewriteTypeCase "String in optional type is replaced"
      (T.optional T.string)
      (T.optional T.int32)]

-- Helper for foo, bar, baz
foo :: TTerm Term
foo = string "foo"

bar :: TTerm Term
bar = string "bar"

baz :: TTerm Term
baz = string "baz"

-- | Test cases for rewriteTerm
-- Using predefined term rewriter: replaceFooWithBar
rewriteTermGroup :: TTerm TestGroup
rewriteTermGroup = subgroup "rewriteTerm" [
    -- Simple terms
    rewriteTermCase "string literal foo replaced with bar"
      foo
      bar,

    rewriteTermCase "string in variable not changed"
      (var "x")
      (var "x"),

    -- Collections
    rewriteTermCase "string in list"
      (list [foo, baz])
      (list [bar, baz]),

    rewriteTermCase "multiple strings in list"
      (list [foo, foo, baz])
      (list [bar, bar, baz]),

    rewriteTermCase "string in optional (just)"
      (optional (just foo))
      (optional (just bar)),

    -- Applications and functions
    rewriteTermCase "string in function application"
      (apply (var "print") foo)
      (apply (var "print") bar),

    rewriteTermCase "string in lambda body"
      (lambda "x" foo)
      (lambda "x" bar),

    rewriteTermCase "string in nested applications"
      (apply (var "f") (apply (var "g") foo))
      (apply (var "f") (apply (var "g") bar)),

    -- Records and products
    rewriteTermCase "string in record field"
      (record (nm "Person") [(nm "name", foo)])
      (record (nm "Person") [(nm "name", bar)]),

    rewriteTermCase "strings in multiple record fields"
      (record (nm "Data") [(nm "a", foo), (nm "b", baz), (nm "c", foo)])
      (record (nm "Data") [(nm "a", bar), (nm "b", baz), (nm "c", bar)]),

    rewriteTermCase "string in pair"
      (pair foo (int32 42))
      (pair bar (int32 42)),

    -- Let bindings
    rewriteTermCase "string in let binding value"
      (letExpr "x" foo (var "x"))
      (letExpr "x" bar (var "x")),

    rewriteTermCase "string in let body"
      (letExpr "x" (int32 1) foo)
      (letExpr "x" (int32 1) bar),

    -- Case statements
    rewriteTermCase "string in first case branch"
      (match (nm "Result") nothing [(nm "success", foo), (nm "error", baz)])
      (match (nm "Result") nothing [(nm "success", bar), (nm "error", baz)]),

    rewriteTermCase "string in second case branch"
      (match (nm "Result") nothing [(nm "success", baz), (nm "error", foo)])
      (match (nm "Result") nothing [(nm "success", baz), (nm "error", bar)]),

    rewriteTermCase "string in default branch"
      (match (nm "Result") (just foo) [(nm "success", baz), (nm "error", baz)])
      (match (nm "Result") (just bar) [(nm "success", baz), (nm "error", baz)]),

    -- Deeply nested
    rewriteTermCase "string deeply nested in record in list in application"
      (apply (var "process") (list [record (nm "Item") [(nm "value", foo)]]))
      (apply (var "process") (list [record (nm "Item") [(nm "value", bar)]])),

    -- Unions and injections
    rewriteTermCase "string in union inject value"
      (inject (nm "Result") "success" foo)
      (inject (nm "Result") "success" bar),

    -- Wrapped terms
    rewriteTermCase "string in wrapped term"
      (wrap (nm "Email") foo)
      (wrap (nm "Email") bar),

    -- Annotated terms
    rewriteTermCase "string in annotated term body"
      (annot emptyAnnMap foo)
      (annot emptyAnnMap bar),

    -- Multiple bindings in let
    rewriteTermCase "string in first of multiple let bindings"
      (multiLet [("x", foo), ("y", baz)] (var "x"))
      (multiLet [("x", bar), ("y", baz)] (var "x")),

    rewriteTermCase "string in second of multiple let bindings"
      (multiLet [("x", baz), ("y", foo)] (var "y"))
      (multiLet [("x", baz), ("y", bar)] (var "y")),

    rewriteTermCase "string in all let bindings and body"
      (multiLet [("x", foo), ("y", foo)] foo)
      (multiLet [("x", bar), ("y", bar)] bar),

    -- Sets
    rewriteTermCase "string in set"
      (set [foo, baz])
      (set [bar, baz]),

    -- Type lambdas and type applications (System F)
    rewriteTermCase "string in type lambda body"
      (tylam "a" foo)
      (tylam "a" bar),

    rewriteTermCase "string in type application body"
      (tyapp foo T.string)
      (tyapp bar T.string),

    rewriteTermCase "string in nested type lambdas"
      (tylam "a" (tylam "b" foo))
      (tylam "a" (tylam "b" bar)),

    -- Annotation edge case: string in annotation subject is replaced, but body stays same
    -- Note: annotations on the annotation map itself are not traversed by rewriteTerm

    -- Complex nested structures
    rewriteTermCase "string in case branch within let binding"
      (letExpr "handler" (match (nm "Result") nothing [(nm "ok", foo), (nm "err", baz)]) (var "handler"))
      (letExpr "handler" (match (nm "Result") nothing [(nm "ok", bar), (nm "err", baz)]) (var "handler")),

    rewriteTermCase "string in annotated wrapped record field"
      (annot emptyAnnMap (wrap (nm "User") (record (nm "UserData") [(nm "name", foo)])))
      (annot emptyAnnMap (wrap (nm "User") (record (nm "UserData") [(nm "name", bar)])))]
