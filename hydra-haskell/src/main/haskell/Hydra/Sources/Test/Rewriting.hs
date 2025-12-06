{-# LANGUAGE OverloadedStrings #-}

-- | Test cases for term rewriting operations (free variables, simplify, flatten let, lift lambda)
module Hydra.Sources.Test.Rewriting where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Coders as Coders
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Terms as T
import qualified Hydra.Dsl.Meta.Types as Ty
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes

import qualified Data.Map as M
import qualified Data.Set as S


module_ :: Module
module_ = Module (Namespace "hydra.test.rewriting") elements
    []
    KernelTypes.kernelTypesModules
    (Just "Test cases for term rewriting operations")
  where
    elements = [el allTestsDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTestsDef :: TBinding TestGroup
allTestsDef = define "allTests" $
    doc "Test cases for term rewriting operations" $
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
      rewriteTermGroup]

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper to build a set of Names
nameSet :: [String] -> TTerm (S.Set Name)
nameSet names = Phantoms.set $ (nm <$> names)

-- Helper for single-binding let
letExpr :: String -> TTerm Term -> TTerm Term -> TTerm Term
letExpr varName value body = T.lets [(nm varName, value)] body

-- Helper for multi-binding let
multiLet :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
multiLet bindings body = T.lets ((\(n, v) -> (nm n, v)) <$> bindings) body

-- | Test cases for free variables computation
freeVariablesGroup :: TTerm TestGroup
freeVariablesGroup = subgroup "freeVariables" [
    freeVarsCase "string literal has no free variables"
      (T.string "foo")
      (nameSet []),

    freeVarsCase "single variable"
      (T.var "x")
      (nameSet ["x"]),

    freeVarsCase "bound variable is not free"
      (T.lambda "y" (T.var "y"))
      (nameSet []),

    freeVarsCase "unbound variable in lambda body"
      (T.lambda "y" (T.var "x"))
      (nameSet ["x"]),

    freeVarsCase "mixed free and bound variables"
      (T.list [
        T.var "x",
        T.apply (T.lambda "y" (T.var "y")) (T.int32 42)])
      (nameSet ["x"]),

    freeVarsCase "multiple free variables"
      (T.list [
        T.var "x",
        T.apply (T.lambda "y" (T.var "y")) (T.var "y")])
      (nameSet ["x", "y"])]

-- | Test cases for term simplification (beta reduction)
simplifyTermGroup :: TTerm TestGroup
simplifyTermGroup = subgroup "simplifyTerm" [
    simplifyCase "const application with literal"
      (T.apply (T.lambda "x" (T.string "foo")) (T.int32 42))
      (T.string "foo"),

    simplifyCase "identity application"
      (T.apply
        (T.lambda "x" (T.list [T.var "x", T.var "x"]))
        (T.var "y"))
      (T.list [T.var "y", T.var "y"]),

    simplifyCase "unused parameter"
      (T.apply (T.lambda "x" (T.string "foo")) (T.var "y"))
      (T.string "foo"),

    simplifyCase "nested lambda applications"
      (T.apply
        (T.lambda "x"
          (T.apply
            (T.lambda "a" (T.list [T.string "foo", T.var "a"]))
            (T.var "x")))
        (T.var "y"))
      (T.list [T.string "foo", T.var "y"])]

-- | Test cases for flattening nested let terms
flattenLetTermsGroup :: TTerm TestGroup
flattenLetTermsGroup = subgroup "flattenLetTerms" [
    flattenCase "non-let term unchanged"
      (T.int32 42)
      (T.int32 42),

    flattenCase "list term unchanged"
      (T.list [T.string "foo"])
      (T.list [T.string "foo"]),

    flattenCase "non-nested let unchanged"
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (T.list [T.var "x", T.var "y"])))
      (letExpr "x" (T.int32 1)
        (letExpr "y" (T.int32 2)
          (T.list [T.var "x", T.var "y"]))),

    -- Nested bindings are flattened with prefix renaming
    flattenCase "nested binding in let value is flattened"
      -- let a = 1; b = (let x = 1; y = 2 in [x, y]) in [a, b]
      (multiLet [
        ("a", T.int32 1),
        ("b", multiLet [("x", T.int32 1), ("y", T.int32 2)]
                (T.list [T.var "x", T.var "y"]))]
        (T.list [T.var "a", T.var "b"]))
      -- let a = 1; b = [b_x, b_y]; b_x = 1; b_y = 2 in [a, b]
      (multiLet [
        ("a", T.int32 1),
        ("b", T.list [T.var "b_x", T.var "b_y"]),
        ("b_x", T.int32 1),
        ("b_y", T.int32 2)]
        (T.list [T.var "a", T.var "b"])),

    -- Multiple levels of nesting
    flattenCase "multiple levels of nesting are flattened"
      -- let a = 1; b = (let x = 1; y = (let p = 137; q = [x, 5] in [a, q]) in [x, y]) in [a, b]
      (multiLet [
        ("a", T.int32 1),
        ("b", multiLet [
          ("x", T.int32 1),
          ("y", multiLet [
            ("p", T.int32 137),
            ("q", T.list [T.var "x", T.int32 5])]
            (T.list [T.var "a", T.var "q"]))]
          (T.list [T.var "x", T.var "y"]))]
        (T.list [T.var "a", T.var "b"]))
      -- Flattened with proper prefixes
      (multiLet [
        ("a", T.int32 1),
        ("b", T.list [T.var "b_x", T.var "b_y"]),
        ("b_x", T.int32 1),
        ("b_y", T.list [T.var "a", T.var "b_y_q"]),
        ("b_y_p", T.int32 137),
        ("b_y_q", T.list [T.var "b_x", T.int32 5])]
        (T.list [T.var "a", T.var "b"]))]

-- | Test cases for lifting lambda above let
liftLambdaAboveLetGroup :: TTerm TestGroup
liftLambdaAboveLetGroup = subgroup "liftLambdaAboveLet" [
    liftLambdaCase "simple let with lambda in body"
      (letExpr "x" (T.int32 42)
        (T.lambda "y" (T.var "x")))
      (T.lambda "y"
        (letExpr "x" (T.int32 42)
          (T.var "x"))),

    liftLambdaCase "bare lambda unchanged"
      (T.lambda "x" (T.var "x"))
      (T.lambda "x" (T.var "x")),

    liftLambdaCase "bare let unchanged"
      (letExpr "x" (T.int32 42) (T.var "x"))
      (letExpr "x" (T.int32 42) (T.var "x")),

    liftLambdaCase "lambda with let in body unchanged"
      (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x")))
      (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x"))),

    liftLambdaCase "let with two nested lambdas"
      (letExpr "x" (T.int32 42)
        (T.lambda "y" (T.lambda "z" (T.var "x"))))
      (T.lambda "y"
        (T.lambda "z"
          (letExpr "x" (T.int32 42) (T.var "x")))),

    liftLambdaCase "lambda inside let body already above let"
      (T.lambda "x"
        (T.lambda "y"
          (letExpr "z" (T.int32 42) (T.var "z"))))
      (T.lambda "x"
        (T.lambda "y"
          (letExpr "z" (T.int32 42) (T.var "z")))),

    -- Multiple bindings and nested lets
    liftLambdaCase "let without lambda in body unchanged"
      (multiLet [("x", T.int32 42), ("y", T.string "hello")]
        (T.pair (T.var "x") (T.var "y")))
      (multiLet [("x", T.int32 42), ("y", T.string "hello")]
        (T.pair (T.var "x") (T.var "y"))),

    liftLambdaCase "multiple let bindings with lambda"
      (multiLet [("x", T.int32 42), ("y", T.string "hello")]
        (T.lambda "z" (T.var "x")))
      (T.lambda "z"
        (multiLet [("x", T.int32 42), ("y", T.string "hello")]
          (T.var "x"))),

    liftLambdaCase "nested lets with lambda at innermost level"
      (letExpr "x" (T.int32 42)
        (letExpr "y" (T.string "hello")
          (T.lambda "z" (T.var "x"))))
      (T.lambda "z"
        (letExpr "x" (T.int32 42)
          (letExpr "y" (T.string "hello")
            (T.var "x")))),

    liftLambdaCase "lambda between two lets"
      (letExpr "x" (T.int32 42)
        (T.lambda "y"
          (letExpr "z" (T.string "hello")
            (T.var "x"))))
      (T.lambda "y"
        (letExpr "x" (T.int32 42)
          (letExpr "z" (T.string "hello")
            (T.var "x")))),

    liftLambdaCase "multiple lambdas between nested lets"
      (letExpr "a" (T.int32 1)
        (T.lambda "x"
          (T.lambda "y"
            (letExpr "b" (T.int32 2)
              (T.var "a")))))
      (T.lambda "x"
        (T.lambda "y"
          (letExpr "a" (T.int32 1)
            (letExpr "b" (T.int32 2)
              (T.var "a"))))),

    liftLambdaCase "multiple lambdas already above let"
      (T.lambda "x"
        (T.lambda "y"
          (letExpr "z" (T.int32 42)
            (T.var "z"))))
      (T.lambda "x"
        (T.lambda "y"
          (letExpr "z" (T.int32 42)
            (T.var "z")))),

    -- Annotation cases
    liftLambdaCase "annotation above let containing lambda"
      (T.annot emptyAnnMap
        (letExpr "x" (T.int32 42) (T.lambda "y" (T.var "x"))))
      (T.annot emptyAnnMap
        (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x")))),

    liftLambdaCase "annotation above lambda in let body"
      (letExpr "x" (T.int32 42)
        (T.annot emptyAnnMap (T.lambda "y" (T.var "x"))))
      (T.lambda "y"
        (T.annot emptyAnnMap (letExpr "x" (T.int32 42) (T.var "x")))),

    liftLambdaCase "annotation between two lambdas"
      (letExpr "x" (T.int32 42)
        (T.lambda "y"
          (T.annot emptyAnnMap (T.lambda "z" (T.var "x")))))
      (T.lambda "y"
        (T.lambda "z"
          (T.annot emptyAnnMap (letExpr "x" (T.int32 42) (T.var "x"))))),

    liftLambdaCase "annotation on the body of lambda in let"
      (letExpr "x" (T.int32 42)
        (T.lambda "y"
          (T.annot emptyAnnMap (T.var "x"))))
      (T.lambda "y"
        (letExpr "x" (T.int32 42)
          (T.annot emptyAnnMap (T.var "x")))),

    liftLambdaCase "annotation on lambda already above let"
      (T.annot emptyAnnMap
        (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x"))))
      (T.annot emptyAnnMap
        (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x")))),

    -- Recursive lifting in nested structures
    liftLambdaCase "let-lambda inside a list"
      (T.list [
        T.int32 1,
        letExpr "x" (T.int32 42) (T.lambda "y" (T.var "x")),
        T.int32 2])
      (T.list [
        T.int32 1,
        T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x")),
        T.int32 2]),

    liftLambdaCase "let-lambda in multiple list elements"
      (T.list [
        letExpr "x" (T.int32 1) (T.lambda "y" (T.var "x")),
        letExpr "z" (T.int32 2) (T.lambda "w" (T.var "z"))])
      (T.list [
        T.lambda "y" (letExpr "x" (T.int32 1) (T.var "x")),
        T.lambda "w" (letExpr "z" (T.int32 2) (T.var "z"))]),

    liftLambdaCase "let-lambda in a let binding value"
      (letExpr "f" (letExpr "x" (T.int32 42) (T.lambda "y" (T.var "x")))
        (T.var "f"))
      (letExpr "f" (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x")))
        (T.var "f")),

    liftLambdaCase "let-lambda inside a pair"
      (T.pair
        (letExpr "x" (T.int32 42) (T.lambda "y" (T.var "x")))
        (T.string "test"))
      (T.pair
        (T.lambda "y" (letExpr "x" (T.int32 42) (T.var "x")))
        (T.string "test")),

    liftLambdaCase "let-lambda in both elements of a pair"
      (T.pair
        (letExpr "x" (T.int32 1) (T.lambda "y" (T.var "x")))
        (letExpr "z" (T.int32 2) (T.lambda "w" (T.var "z"))))
      (T.pair
        (T.lambda "y" (letExpr "x" (T.int32 1) (T.var "x")))
        (T.lambda "w" (letExpr "z" (T.int32 2) (T.var "z")))),

    liftLambdaCase "let-lambda inside lambda body"
      (T.lambda "outer"
        (letExpr "x" (T.int32 42) (T.lambda "inner" (T.var "x"))))
      (T.lambda "outer"
        (T.lambda "inner" (letExpr "x" (T.int32 42) (T.var "x"))))]

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- | Test cases for deannotating terms (stripping top-level annotations)
-- Note: deannotateTerm only strips annotations at the top level, not recursively
deannotateTermGroup :: TTerm TestGroup
deannotateTermGroup = subgroup "deannotateTerm" [
    deannotateTermCase "unannotated literal unchanged"
      (T.int32 42)
      (T.int32 42),

    deannotateTermCase "unannotated variable unchanged"
      (T.var "x")
      (T.var "x"),

    deannotateTermCase "unannotated lambda unchanged"
      (T.lambda "x" (T.var "x"))
      (T.lambda "x" (T.var "x")),

    deannotateTermCase "single annotation stripped"
      (T.annot emptyAnnMap (T.int32 42))
      (T.int32 42),

    deannotateTermCase "nested annotations stripped"
      (T.annot emptyAnnMap (T.annot emptyAnnMap (T.int32 42)))
      (T.int32 42),

    deannotateTermCase "annotated lambda stripped"
      (T.annot emptyAnnMap (T.lambda "x" (T.var "x")))
      (T.lambda "x" (T.var "x")),

    deannotateTermCase "annotated application stripped"
      (T.annot emptyAnnMap (T.apply (T.var "f") (T.var "x")))
      (T.apply (T.var "f") (T.var "x"))]

-- | Test cases for deannotating types (stripping top-level annotations)
-- Note: deannotateType only strips annotations at the top level, not recursively
deannotateTypeGroup :: TTerm TestGroup
deannotateTypeGroup = subgroup "deannotateType" [
    deannotateTypeCase "unannotated primitive type unchanged"
      Ty.int32
      Ty.int32,

    deannotateTypeCase "unannotated string type unchanged"
      Ty.string
      Ty.string,

    deannotateTypeCase "unannotated function type unchanged"
      (Ty.function Ty.int32 Ty.string)
      (Ty.function Ty.int32 Ty.string),

    deannotateTypeCase "single annotation stripped"
      (Ty.annot emptyAnnMap Ty.int32)
      Ty.int32,

    deannotateTypeCase "nested annotations stripped"
      (Ty.annot emptyAnnMap (Ty.annot emptyAnnMap Ty.string))
      Ty.string,

    deannotateTypeCase "annotated list type stripped"
      (Ty.annot emptyAnnMap (Ty.list Ty.int32))
      (Ty.list Ty.int32),

    deannotateTypeCase "annotated function type stripped"
      (Ty.annot emptyAnnMap (Ty.function Ty.int32 Ty.string))
      (Ty.function Ty.int32 Ty.string)]

-- | Test cases for topological sort of bindings
-- The function topologicalSortBindingMap takes a map of (name -> term) bindings
-- and returns groups of bindings in topological order, where each group contains
-- mutually recursive bindings (strongly connected components).
topologicalSortBindingsGroup :: TTerm TestGroup
topologicalSortBindingsGroup = subgroup "topologicalSortBindings" [
    sortBindingsCase "isolated bindings"
      (Phantoms.list [
        Phantoms.pair (nm "a") (T.string "foo"),
        Phantoms.pair (nm "b") (T.string "bar")])
      (Phantoms.list [
        Phantoms.list [Phantoms.pair (nm "a") (T.string "foo")],
        Phantoms.list [Phantoms.pair (nm "b") (T.string "bar")]]),

    sortBindingsCase "single recursive binding"
      (Phantoms.list [
        Phantoms.pair (nm "a") (T.list [T.var "a"])])
      (Phantoms.list [
        Phantoms.list [Phantoms.pair (nm "a") (T.list [T.var "a"])]]),

    sortBindingsCase "mutually recursive bindings"
      (Phantoms.list [
        Phantoms.pair (nm "a") (T.list [T.var "b"]),
        Phantoms.pair (nm "b") (T.list [T.var "a"])])
      (Phantoms.list [
        Phantoms.list [
          Phantoms.pair (nm "a") (T.list [T.var "b"]),
          Phantoms.pair (nm "b") (T.list [T.var "a"])]]),

    sortBindingsCase "mixed bindings"
      (Phantoms.list [
        Phantoms.pair (nm "a") (T.var "b"),
        Phantoms.pair (nm "b") (T.list [T.var "a", T.var "c"]),
        Phantoms.pair (nm "c") (T.string "foo"),
        Phantoms.pair (nm "d") (T.string "bar")])
      (Phantoms.list [
        Phantoms.list [Phantoms.pair (nm "c") (T.string "foo")],
        Phantoms.list [
          Phantoms.pair (nm "a") (T.var "b"),
          Phantoms.pair (nm "b") (T.list [T.var "a", T.var "c"])],
        Phantoms.list [Phantoms.pair (nm "d") (T.string "bar")]])]

-- | Test cases for normalizing type variables in terms
-- The function normalizeTypeVariablesInTerm renames type variables to a canonical form (t0, t1, t2, etc.)
normalizeTypeVariablesGroup :: TTerm TestGroup
normalizeTypeVariablesGroup = subgroup "normalizeTypeVariables" [
    -- No type variables - terms should remain unchanged
    normalizeTypeVarsCase "literal without type variables unchanged"
      (T.int32 42)
      (T.int32 42),

    normalizeTypeVarsCase "simple let without type annotations unchanged"
      (letExpr "foo" (T.string "foo") (T.int32 42))
      (letExpr "foo" (T.string "foo") (T.int32 42)),

    normalizeTypeVarsCase "let with monomorphic type scheme unchanged"
      (T.letsTyped [("foo", T.string "foo", Ty.mono Ty.string)] (T.int32 42))
      (T.letsTyped [("foo", T.string "foo", Ty.mono Ty.string)] (T.int32 42)),

    normalizeTypeVarsCase "let with monomorphic binding referencing string"
      (T.letsTyped [("foo", T.string "foo", Ty.mono Ty.string)] (T.int32 42))
      (T.letsTyped [("foo", T.string "foo", Ty.mono Ty.string)] (T.int32 42)),

    -- Only free type variables - no normalization needed
    normalizeTypeVarsCase "polymorphic binding with free type variable unchanged"
      (T.letsTyped [("foo", T.var "bar", Ty.mono (Ty.var "a"))] (T.int32 42))
      (T.letsTyped [("foo", T.var "bar", Ty.mono (Ty.var "a"))] (T.int32 42)),

    normalizeTypeVarsCase "monomorphic binding with typed lambda unchanged"
      (T.letsTyped [("foo", T.string "foo", Ty.mono Ty.string)]
        (T.lambdaTyped "x" (Ty.function (Ty.var "a") Ty.int32) (T.int32 42)))
      (T.letsTyped [("foo", T.string "foo", Ty.mono Ty.string)]
        (T.lambdaTyped "x" (Ty.function (Ty.var "a") Ty.int32) (T.int32 42))),

    normalizeTypeVarsCase "polymorphic binding with typed lambda in body unchanged"
      (T.letsTyped [("foo", T.var "bar", Ty.mono (Ty.var "a"))]
        (T.lambdaTyped "x" (Ty.function (Ty.var "a") Ty.int32) (T.int32 42)))
      (T.letsTyped [("foo", T.var "bar", Ty.mono (Ty.var "a"))]
        (T.lambdaTyped "x" (Ty.function (Ty.var "a") Ty.int32) (T.int32 42))),

    -- Polymorphic let bindings should have type variables normalized
    normalizeTypeVarsCase "polymorphic identity function normalized"
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
        (T.apply (T.var "id") (T.int32 42)))
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
        (T.apply (T.var "id") (T.int32 42))),

    normalizeTypeVarsCase "polymorphic const function normalized"
      (T.letsTyped [("const", T.lambda "x" (T.lambda "y" (T.var "x")),
        Ty.poly ["a", "b"] (Ty.function (Ty.var "a") (Ty.function (Ty.var "b") (Ty.var "a"))))]
        (T.apply (T.apply (T.var "const") (T.int32 42)) (T.string "foo")))
      (T.letsTyped [("const", T.lambda "x" (T.lambda "y" (T.var "x")),
        Ty.poly ["t0", "t1"] (Ty.function (Ty.var "t0") (Ty.function (Ty.var "t1") (Ty.var "t0"))))]
        (T.apply (T.apply (T.var "const") (T.int32 42)) (T.string "foo"))),

    -- Rewriting of bindings does not affect body (free variable in body coincides with bound variable in binding)
    normalizeTypeVarsCase "binding rewriting does not affect body with typed lambda"
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
        (T.lambdaTyped "x" (Ty.function (Ty.var "a") Ty.int32) (T.int32 42)))
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
        (T.lambdaTyped "x" (Ty.function (Ty.var "a") Ty.int32) (T.int32 42))),

    -- Nested polymorphic let bindings - each type scheme is normalized independently
    normalizeTypeVarsCase "nested polymorphic lets normalized"
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
        (T.letsTyped [("id2", T.lambda "y" (T.var "y"),
          Ty.poly ["b"] (Ty.function (Ty.var "b") (Ty.var "b")))]
          (T.apply (T.var "id") (T.apply (T.var "id2") (T.int32 42)))))
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
        (T.letsTyped [("id2", T.lambda "y" (T.var "y"),
          Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
          (T.apply (T.var "id") (T.apply (T.var "id2") (T.int32 42))))),

    normalizeTypeVarsCase "nested same substitution in bindings and environment"
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
        (T.letsTyped [("id2", T.lambda "x" (T.var "x"),
          Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
          (T.apply (T.var "id") (T.int32 42))))
      (T.letsTyped [("id", T.lambda "x" (T.var "x"),
        Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
        (T.letsTyped [("id2", T.lambda "x" (T.var "x"),
          Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
          (T.apply (T.var "id") (T.int32 42)))),

    -- Parent variable shadows child variable in nested lets with typed lambdas
    normalizeTypeVarsCase "parent type variable shadows child variable"
      (T.letsTyped [("id",
        T.letsTyped [("id2",
          T.lambdaTyped "x" (Ty.var "a") (T.var "x"),
          Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
          (T.lambdaTyped "y" (Ty.var "a") (T.apply (T.var "id2") (T.var "y"))),
        Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
        (T.apply (T.var "id") (T.int32 42)))
      (T.letsTyped [("id",
        T.letsTyped [("id2",
          T.lambdaTyped "x" (Ty.var "t1") (T.var "x"),
          Ty.poly ["t1"] (Ty.function (Ty.var "t1") (Ty.var "t1")))]
          (T.lambdaTyped "y" (Ty.var "t0") (T.apply (T.var "id2") (T.var "y"))),
        Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
        (T.apply (T.var "id") (T.int32 42))),

    -- No shadowing: distinct type variable names in nested lets
    normalizeTypeVarsCase "no shadowing distinct type variables"
      (T.letsTyped [("id",
        T.letsTyped [("id2",
          T.lambdaTyped "x" (Ty.var "b") (T.var "x"),
          Ty.poly ["b"] (Ty.function (Ty.var "b") (Ty.var "b")))]
          (T.lambdaTyped "y" (Ty.var "a") (T.apply (T.var "id2") (T.var "y"))),
        Ty.poly ["a"] (Ty.function (Ty.var "a") (Ty.var "a")))]
        (T.apply (T.var "id") (T.int32 42)))
      (T.letsTyped [("id",
        T.letsTyped [("id2",
          T.lambdaTyped "x" (Ty.var "t1") (T.var "x"),
          Ty.poly ["t1"] (Ty.function (Ty.var "t1") (Ty.var "t1")))]
          (T.lambdaTyped "y" (Ty.var "t0") (T.apply (T.var "id2") (T.var "y"))),
        Ty.poly ["t0"] (Ty.function (Ty.var "t0") (Ty.var "t0")))]
        (T.apply (T.var "id") (T.int32 42))),

    -- Complex: locally free type variable in nested binding
    normalizeTypeVarsCase "locally free type variable in nested binding"
      (T.letsTyped [("fun1",
        T.lambdaTyped "x" (Ty.var "a") (T.lambdaTyped "y" (Ty.var "b")
          (T.letsTyped [("fun2",
            T.lambdaTyped "z" (Ty.var "c") (T.pair (T.var "z") (T.var "y")),
            Ty.poly ["c"] (Ty.function (Ty.var "c") (Ty.pair (Ty.var "c") (Ty.var "b"))))]
            (T.apply (T.var "fun2") (T.var "x")))),
        Ty.poly ["a", "b"] (Ty.function (Ty.var "a") (Ty.function (Ty.var "b") (Ty.pair (Ty.var "a") (Ty.var "b")))))]
        (T.apply (T.apply (T.var "fun1") (T.string "foo")) (T.int32 42)))
      (T.letsTyped [("fun1",
        T.lambdaTyped "x" (Ty.var "t0") (T.lambdaTyped "y" (Ty.var "t1")
          (T.letsTyped [("fun2",
            T.lambdaTyped "z" (Ty.var "t2") (T.pair (T.var "z") (T.var "y")),
            Ty.poly ["t2"] (Ty.function (Ty.var "t2") (Ty.pair (Ty.var "t2") (Ty.var "t1"))))]
            (T.apply (T.var "fun2") (T.var "x")))),
        Ty.poly ["t0", "t1"] (Ty.function (Ty.var "t0") (Ty.function (Ty.var "t1") (Ty.pair (Ty.var "t0") (Ty.var "t1")))))]
        (T.apply (T.apply (T.var "fun1") (T.string "foo")) (T.int32 42)))]

-- | Test cases for eta expansion of terms
-- Eta expansion adds explicit lambda wrappers to partially applied functions
etaExpandTermGroup :: TTerm TestGroup
etaExpandTermGroup = subgroup "etaExpandTerm" [
    -- Terms that don't expand (already saturated or not functions)
    etaCase "integer literal unchanged"
      (T.int32 42)
      (T.int32 42),

    etaCase "string list unchanged"
      (T.list [T.string "foo", T.string "bar"])
      (T.list [T.string "foo", T.string "bar"]),

    etaCase "fully applied binary function unchanged"
      (T.apply (T.apply (T.primitive _strings_splitOn) (T.string "foo")) (T.string "bar"))
      (T.apply (T.apply (T.primitive _strings_splitOn) (T.string "foo")) (T.string "bar")),

    -- Lambda with fully applied primitive using a string literal (matches EtaExpansion.hs pattern)
    etaCase "lambda with fully applied primitive unchanged"
      (T.lambda "x" (T.apply (T.apply (T.primitive _strings_splitOn) (T.string ",")) (T.var "x")))
      (T.lambda "x" (T.apply (T.apply (T.primitive _strings_splitOn) (T.string ",")) (T.var "x"))),

    etaCase "lambda returning constant unchanged"
      (T.lambda "x" (T.int32 42))
      (T.lambda "x" (T.int32 42)),

    -- Bare primitives are NOT expanded (they stay as-is)
    etaCase "bare unary primitive unchanged"
      (T.primitive _strings_toLower)
      (T.primitive _strings_toLower),

    etaCase "bare binary primitive unchanged"
      (T.primitive _strings_splitOn)
      (T.primitive _strings_splitOn),

    etaCase "partially applied binary primitive expands to one lambda"
      (T.apply (T.primitive _strings_splitOn) (T.var "foo"))
      (T.lambda "v1" (T.apply (T.apply (T.primitive _strings_splitOn) (T.var "foo")) (T.var "v1"))),

    etaCase "projection expands to lambda"
      (T.project (nm "Person") (nm "firstName"))
      (T.lambda "v1" (T.apply (T.project (nm "Person") (nm "firstName")) (T.var "v1"))),

    -- Subterms within applications
    etaCase "partial application inside lambda expands"
      (T.lambda "x" (T.apply (T.primitive _strings_splitOn) (T.var "x")))
      (T.lambda "x" (T.lambda "v1" (T.apply (T.apply (T.primitive _strings_splitOn) (T.var "x")) (T.var "v1")))),

    -- Let bindings
    etaCase "let with constant body unchanged"
      (letExpr "foo" (T.int32 137) (T.int32 42))
      (letExpr "foo" (T.int32 137) (T.int32 42)),

    etaCase "let with bare primitive value unchanged"
      (letExpr "foo" (T.primitive _strings_splitOn) (T.var "foo"))
      (letExpr "foo" (T.primitive _strings_splitOn) (T.var "foo")),

    -- Complete applications are no-ops
    etaCase "fully applied unary unchanged"
      (T.apply (T.primitive _strings_toLower) (T.string "FOO"))
      (T.apply (T.primitive _strings_toLower) (T.string "FOO")),

    -- Subterms
    etaCase "partial application in list expands"
      (T.list [T.lambda "x" (T.list [T.string "foo"]), T.apply (T.primitive _strings_splitOn) (T.string "bar")])
      (T.list [T.lambda "x" (T.list [T.string "foo"]), T.lambda "v1" (T.apply (T.apply (T.primitive _strings_splitOn) (T.string "bar")) (T.var "v1"))])]

-- Helper to create labeled node (pair of label and list of children)
labeledNode :: String -> [TTerm Term] -> TTerm Term
labeledNode label children = T.pair (T.string label) (T.list children)

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
      (T.list [T.string "a"]),

    foldOverTermCase "collect labels from tree - pre-order"
      (labeledNode "a" [labeledNode "b" [], labeledNode "c" [labeledNode "d" []]])
      Coders.traversalOrderPre
      foldOpCollectLabels
      (T.list [T.string "a", T.string "b", T.string "c", T.string "d"]),

    foldOverTermCase "collect labels from single node - post-order"
      (labeledNode "a" [])
      Coders.traversalOrderPost
      foldOpCollectLabels
      (T.list [T.string "a"]),

    foldOverTermCase "collect labels from tree - post-order"
      (labeledNode "a" [labeledNode "b" [], labeledNode "c" [labeledNode "d" []]])
      Coders.traversalOrderPost
      foldOpCollectLabels
      (T.list [T.string "b", T.string "d", T.string "c", T.string "a"]),

    -- sumInt32Literals tests (from testFoldOverTerm in RewritingSpec.hs)
    foldOverTermCase "sum int32 literals"
      (T.list [T.int32 42, T.apply (T.lambda "x" (T.var "x")) (T.int32 10)])
      Coders.traversalOrderPre
      foldOpSumInt32Literals
      (T.int32 52),

    -- collectListLengths tests (from testFoldOverTerm in RewritingSpec.hs)
    foldOverTermCase "collect list lengths - pre-order"
      (T.list [T.list [T.string "foo", T.string "bar"], T.apply (T.lambda "x" (T.var "x")) (T.list [T.string "quux"])])
      Coders.traversalOrderPre
      foldOpCollectListLengths
      (T.list [T.int32 2, T.int32 2, T.int32 1]),

    foldOverTermCase "collect list lengths - post-order"
      (T.list [T.list [T.string "foo", T.string "bar"], T.apply (T.lambda "x" (T.var "x")) (T.list [T.string "quux"])])
      Coders.traversalOrderPost
      foldOpCollectListLengths
      (T.list [T.int32 2, T.int32 1, T.int32 2])]

-- | Test cases for rewriteType
-- Using predefined type rewriter: replaceStringWithInt32
rewriteTypeGroup :: TTerm TestGroup
rewriteTypeGroup = subgroup "rewriteType" [
    rewriteTypeCase "String type in left side of either is replaced"
      (Ty.either_ Ty.string Ty.int32)
      (Ty.either_ Ty.int32 Ty.int32),

    rewriteTypeCase "String type in right side of either is replaced"
      (Ty.either_ Ty.int32 Ty.string)
      (Ty.either_ Ty.int32 Ty.int32),

    rewriteTypeCase "String types in both sides of either are replaced"
      (Ty.either_ Ty.string Ty.string)
      (Ty.either_ Ty.int32 Ty.int32),

    rewriteTypeCase "String type in nested either (left of left) is replaced"
      (Ty.either_ (Ty.either_ Ty.string Ty.int32) Ty.int64)
      (Ty.either_ (Ty.either_ Ty.int32 Ty.int32) Ty.int64),

    rewriteTypeCase "String type in nested either (right of right) is replaced"
      (Ty.either_ Ty.int64 (Ty.either_ Ty.int32 Ty.string))
      (Ty.either_ Ty.int64 (Ty.either_ Ty.int32 Ty.int32)),

    rewriteTypeCase "String types in complex nested either are all replaced"
      (Ty.either_ (Ty.either_ Ty.string Ty.string) (Ty.either_ Ty.string Ty.int64))
      (Ty.either_ (Ty.either_ Ty.int32 Ty.int32) (Ty.either_ Ty.int32 Ty.int64)),

    rewriteTypeCase "String in list type is replaced"
      (Ty.list Ty.string)
      (Ty.list Ty.int32),

    rewriteTypeCase "String in function domain is replaced"
      (Ty.function Ty.string Ty.int64)
      (Ty.function Ty.int32 Ty.int64),

    rewriteTypeCase "String in function codomain is replaced"
      (Ty.function Ty.int64 Ty.string)
      (Ty.function Ty.int64 Ty.int32),

    rewriteTypeCase "String in optional type is replaced"
      (Ty.optional Ty.string)
      (Ty.optional Ty.int32)]

-- Helper for foo, bar, baz
foo :: TTerm Term
foo = T.string "foo"

bar :: TTerm Term
bar = T.string "bar"

baz :: TTerm Term
baz = T.string "baz"

-- | Test cases for rewriteTerm
-- Using predefined term rewriter: replaceFooWithBar
rewriteTermGroup :: TTerm TestGroup
rewriteTermGroup = subgroup "rewriteTerm" [
    -- Simple terms
    rewriteTermCase "string literal foo replaced with bar"
      foo
      bar,

    rewriteTermCase "string in variable not changed"
      (T.var "x")
      (T.var "x"),

    -- Collections
    rewriteTermCase "string in list"
      (T.list [foo, baz])
      (T.list [bar, baz]),

    rewriteTermCase "multiple strings in list"
      (T.list [foo, foo, baz])
      (T.list [bar, bar, baz]),

    rewriteTermCase "string in optional (just)"
      (T.optional (T.just foo))
      (T.optional (T.just bar)),

    -- Applications and functions
    rewriteTermCase "string in function application"
      (T.apply (T.var "print") foo)
      (T.apply (T.var "print") bar),

    rewriteTermCase "string in lambda body"
      (T.lambda "x" foo)
      (T.lambda "x" bar),

    rewriteTermCase "string in nested applications"
      (T.apply (T.var "f") (T.apply (T.var "g") foo))
      (T.apply (T.var "f") (T.apply (T.var "g") bar)),

    -- Records and products
    rewriteTermCase "string in record field"
      (T.record (nm "Person") [(nm "name", foo)])
      (T.record (nm "Person") [(nm "name", bar)]),

    rewriteTermCase "strings in multiple record fields"
      (T.record (nm "Data") [(nm "a", foo), (nm "b", baz), (nm "c", foo)])
      (T.record (nm "Data") [(nm "a", bar), (nm "b", baz), (nm "c", bar)]),

    rewriteTermCase "string in pair"
      (T.pair foo (T.int32 42))
      (T.pair bar (T.int32 42)),

    -- Let bindings
    rewriteTermCase "string in let binding value"
      (letExpr "x" foo (T.var "x"))
      (letExpr "x" bar (T.var "x")),

    rewriteTermCase "string in let body"
      (letExpr "x" (T.int32 1) foo)
      (letExpr "x" (T.int32 1) bar),

    -- Case statements
    rewriteTermCase "string in first case branch"
      (T.match (nm "Result") T.nothing [(nm "success", foo), (nm "error", baz)])
      (T.match (nm "Result") T.nothing [(nm "success", bar), (nm "error", baz)]),

    rewriteTermCase "string in second case branch"
      (T.match (nm "Result") T.nothing [(nm "success", baz), (nm "error", foo)])
      (T.match (nm "Result") T.nothing [(nm "success", baz), (nm "error", bar)]),

    rewriteTermCase "string in default branch"
      (T.match (nm "Result") (T.just foo) [(nm "success", baz), (nm "error", baz)])
      (T.match (nm "Result") (T.just bar) [(nm "success", baz), (nm "error", baz)]),

    -- Deeply nested
    rewriteTermCase "string deeply nested in record in list in application"
      (T.apply (T.var "process") (T.list [T.record (nm "Item") [(nm "value", foo)]]))
      (T.apply (T.var "process") (T.list [T.record (nm "Item") [(nm "value", bar)]])),

    -- Unions and injections
    rewriteTermCase "string in union inject value"
      (T.inject (nm "Result") "success" foo)
      (T.inject (nm "Result") "success" bar),

    -- Wrapped terms
    rewriteTermCase "string in wrapped term"
      (T.wrap (nm "Email") foo)
      (T.wrap (nm "Email") bar),

    -- Annotated terms
    rewriteTermCase "string in annotated term body"
      (T.annot emptyAnnMap foo)
      (T.annot emptyAnnMap bar),

    -- Multiple bindings in let
    rewriteTermCase "string in first of multiple let bindings"
      (multiLet [("x", foo), ("y", baz)] (T.var "x"))
      (multiLet [("x", bar), ("y", baz)] (T.var "x")),

    rewriteTermCase "string in second of multiple let bindings"
      (multiLet [("x", baz), ("y", foo)] (T.var "y"))
      (multiLet [("x", baz), ("y", bar)] (T.var "y")),

    rewriteTermCase "string in all let bindings and body"
      (multiLet [("x", foo), ("y", foo)] foo)
      (multiLet [("x", bar), ("y", bar)] bar),

    -- Sets
    rewriteTermCase "string in set"
      (T.set [foo, baz])
      (T.set [bar, baz]),

    -- Type lambdas and type applications (System F)
    rewriteTermCase "string in type lambda body"
      (T.tylam "a" foo)
      (T.tylam "a" bar),

    rewriteTermCase "string in type application body"
      (T.tyapp foo Ty.string)
      (T.tyapp bar Ty.string),

    rewriteTermCase "string in nested type lambdas"
      (T.tylam "a" (T.tylam "b" foo))
      (T.tylam "a" (T.tylam "b" bar)),

    -- Annotation edge case: string in annotation subject is replaced, but body stays same
    -- Note: annotations on the annotation map itself are not traversed by rewriteTerm

    -- Complex nested structures
    rewriteTermCase "string in case branch within let binding"
      (letExpr "handler" (T.match (nm "Result") T.nothing [(nm "ok", foo), (nm "err", baz)]) (T.var "handler"))
      (letExpr "handler" (T.match (nm "Result") T.nothing [(nm "ok", bar), (nm "err", baz)]) (T.var "handler")),

    rewriteTermCase "string in annotated wrapped record field"
      (T.annot emptyAnnMap (T.wrap (nm "User") (T.record (nm "UserData") [(nm "name", foo)])))
      (T.annot emptyAnnMap (T.wrap (nm "User") (T.record (nm "UserData") [(nm "name", bar)])))]
