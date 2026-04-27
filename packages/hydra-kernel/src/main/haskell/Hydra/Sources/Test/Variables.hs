{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for variable analysis and manipulation (free variables, unshadowing, normalization)
module Hydra.Sources.Test.Variables where

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
import qualified Hydra.Sources.Kernel.Terms.Variables as VariablesModule
import qualified Hydra.Dsl.Meta.Lib.Sets as Sets
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings

import qualified Data.Set                     as S


ns :: Namespace
ns = Namespace "hydra.test.variables"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [ShowCore.ns, VariablesModule.ns, TestGraph.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = (Just "Test cases for variable analysis and manipulation")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Local alias for polymorphic application (Phantoms.@@ applies TBindings; Terms.@@ only works on TTerm Term)
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- Field constructor for cases/match (uses Phantoms.>>: to create Field, since the unqualified >>: from Testing creates tuples)
(~>:) :: AsTerm t a => Name -> t -> Field
(~>:) = (Phantoms.>>:)
infixr 0 ~>:

-- | Show a term as a string using ShowCore.term
showTerm :: TTerm Term -> TTerm String
showTerm t = ShowCore.term # t

-- | Show a set of names as a sorted, comma-separated string: "{name1, name2, ...}"
showNameSet :: TTerm (S.Set Name) -> TTerm String
showNameSet s = Strings.cat $ plist [
  pstring "{",
  Strings.intercalate (pstring ", ") (Lists.map (plambda "n" (Core.unName (pvar "n"))) (Sets.toList s)),
  pstring "}"]
  where
    plist = Phantoms.list; pstring = Phantoms.string; plambda = Phantoms.lambda; pvar = Phantoms.var

-- | Helper for Term -> Term kernel function test cases
termCase :: String -> TTermDefinition (Term -> Term) -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
termCase cname func input output = universalCase cname (showTerm (func # input)) (showTerm output)

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

-- | Convenience helpers for specific kernel functions
unshadowCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
unshadowCase cname = termCase cname VariablesModule.unshadowVariables

normalizeTypeVarsCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
normalizeTypeVarsCase cname = termCase cname VariablesModule.normalizeTypeVariablesInTerm

freeVarsCase :: String -> TTerm Term -> TTerm (S.Set Name) -> TTerm TestCaseWithMetadata
freeVarsCase cname input expected = universalCase cname
  (showNameSet (VariablesModule.freeVariablesInTerm # input))
  (showNameSet expected)

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

-- | Test cases for unshadowVariables
-- The function renames shadowed variables by appending a counter suffix.
-- When a variable name is first introduced (lambda or let), it keeps its name.
-- When the same name is introduced again in an inner scope (shadowing), the inner
-- binding is renamed to name<counter> (e.g., x2, x3, ...), and references in
-- the inner body are updated accordingly.
unshadowVariablesGroup :: TTerm TestGroup
unshadowVariablesGroup = subgroup "unshadowVariables" [

    -- === No shadowing: terms should be unchanged ===

    unshadowCase "literal unchanged"
      (int32 42)
      (int32 42),

    unshadowCase "variable unchanged"
      (var "x")
      (var "x"),

    unshadowCase "single lambda unchanged"
      (lambda "x" (var "x"))
      (lambda "x" (var "x")),

    unshadowCase "distinct lambda parameters unchanged"
      (lambda "x" (lambda "y" (list [var "x", var "y"])))
      (lambda "x" (lambda "y" (list [var "x", var "y"]))),

    unshadowCase "let with no shadowing unchanged"
      (letExpr "x" (int32 1) (var "x"))
      (letExpr "x" (int32 1) (var "x")),

    unshadowCase "let and lambda with distinct names unchanged"
      (letExpr "x" (int32 1) (lambda "y" (list [var "x", var "y"])))
      (letExpr "x" (int32 1) (lambda "y" (list [var "x", var "y"]))),

    -- === Simple lambda shadowing ===

    unshadowCase "inner lambda shadows outer lambda"
      (lambda "x" (lambda "x" (var "x")))
      (lambda "x" (lambda "x2" (var "x2"))),

    unshadowCase "inner lambda shadows outer - body references both"
      (lambda "x" (list [var "x", lambda "x" (var "x")]))
      (lambda "x" (list [var "x", lambda "x2" (var "x2")])),

    unshadowCase "triple nested lambda same name"
      (lambda "x" (lambda "x" (lambda "x" (var "x"))))
      (lambda "x" (lambda "x2" (lambda "x3" (var "x3")))),

    -- === Lambda shadowing with different parameters ===

    unshadowCase "two parameters shadow sequentially"
      (lambda "x" (lambda "y" (lambda "x" (lambda "y" (list [var "x", var "y"])))))
      (lambda "x" (lambda "y" (lambda "x2" (lambda "y2" (list [var "x2", var "y2"]))))),

    -- === Let introduces names that lambdas can shadow ===

    unshadowCase "lambda shadows let-bound variable"
      (letExpr "x" (int32 1) (lambda "x" (var "x")))
      (letExpr "x" (int32 1) (lambda "x2" (var "x2"))),

    unshadowCase "lambda shadows one of multiple let bindings"
      (multiLet [("x", int32 1), ("y", int32 2)]
        (lambda "x" (list [var "x", var "y"])))
      (multiLet [("x", int32 1), ("y", int32 2)]
        (lambda "x2" (list [var "x2", var "y"]))),

    -- === Nested lets ===

    unshadowCase "inner let body with lambda shadowing outer let"
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (lambda "x" (var "x"))))
      (letExpr "x" (int32 1)
        (letExpr "y" (int32 2)
          (lambda "x2" (var "x2")))),

    -- === Shadowing inside application ===

    unshadowCase "shadowed lambda in function position of application"
      (lambda "f" (apply (lambda "f" (var "f")) (var "f")))
      (lambda "f" (apply (lambda "f2" (var "f2")) (var "f"))),

    -- === Shadowing inside list ===

    unshadowCase "shadowed lambdas in list elements"
      (lambda "x"
        (list [
          lambda "x" (var "x"),
          lambda "x" (var "x")]))
      (lambda "x"
        (list [
          lambda "x2" (var "x2"),
          lambda "x2" (var "x2")])),

    -- === Shadowing inside record ===

    unshadowCase "shadowed lambda in record field"
      (lambda "x"
        (record (nm "Pair") [
          (nm "fst", lambda "x" (var "x")),
          (nm "snd", var "x")]))
      (lambda "x"
        (record (nm "Pair") [
          (nm "fst", lambda "x2" (var "x2")),
          (nm "snd", var "x")])),

    -- === Shadowing inside case/match branches ===

    unshadowCase "shadowed lambda in case branch"
      (lambda "x"
        (match (nm "Maybe") nothing [
          (nm "nothing", int32 0),
          (nm "just", lambda "x" (var "x"))]))
      (lambda "x"
        (match (nm "Maybe") nothing [
          (nm "nothing", int32 0),
          (nm "just", lambda "x2" (var "x2"))])),

    -- === Shadowing inside pair ===

    unshadowCase "shadowed lambda in pair"
      (lambda "x" (pair (lambda "x" (var "x")) (var "x")))
      (lambda "x" (pair (lambda "x2" (var "x2")) (var "x"))),

    -- === Shadowing inside optional ===

    unshadowCase "shadowed lambda inside optional"
      (lambda "x" (optional (just (lambda "x" (var "x")))))
      (lambda "x" (optional (just (lambda "x2" (var "x2"))))),

    -- === Shadowing inside set ===

    unshadowCase "shadowed lambda inside set element"
      (lambda "x" (set [lambda "x" (var "x")]))
      (lambda "x" (set [lambda "x2" (var "x2")])),

    -- === Shadowing inside inject (union construction) ===

    unshadowCase "shadowed lambda in union injection"
      (lambda "x" (inject (nm "Result") "ok" (lambda "x" (var "x"))))
      (lambda "x" (inject (nm "Result") "ok" (lambda "x2" (var "x2")))),

    -- === Shadowing inside wrap ===

    unshadowCase "shadowed lambda inside wrapped term"
      (lambda "x" (wrap (nm "Age") (lambda "x" (var "x"))))
      (lambda "x" (wrap (nm "Age") (lambda "x2" (var "x2")))),

    -- === Shadowing inside type lambda ===

    unshadowCase "shadowed lambda inside type lambda"
      (lambda "x" (tylam "a" (lambda "x" (var "x"))))
      (lambda "x" (tylam "a" (lambda "x2" (var "x2")))),

    -- === Shadowing inside type application ===

    unshadowCase "shadowed lambda inside type application"
      (lambda "x" (tyapp (lambda "x" (var "x")) T.string))
      (lambda "x" (tyapp (lambda "x2" (var "x2")) T.string)),

    -- === Shadowing inside annotation ===

    unshadowCase "shadowed lambda inside annotated term"
      (lambda "x" (annot emptyAnnMap (lambda "x" (var "x"))))
      (lambda "x" (annot emptyAnnMap (lambda "x2" (var "x2")))),

    -- === Complex nesting ===

    unshadowCase "shadowing at multiple depths"
      (lambda "x"
        (lambda "y"
          (lambda "x"
            (lambda "y"
              (list [var "x", var "y"])))))
      (lambda "x"
        (lambda "y"
          (lambda "x2"
            (lambda "y2"
              (list [var "x2", var "y2"]))))),

    unshadowCase "let then lambda then lambda all same name"
      (letExpr "x" (int32 1)
        (lambda "x" (lambda "x" (var "x"))))
      (letExpr "x" (int32 1)
        (lambda "x2" (lambda "x3" (var "x3")))),

    unshadowCase "lambda with shadowing in let binding value"
      (lambda "x"
        (letExpr "y" (lambda "x" (var "x"))
          (apply (var "y") (var "x"))))
      (lambda "x"
        (letExpr "y" (lambda "x2" (var "x2"))
          (apply (var "y") (var "x")))),

    -- === No-op cases: terms without binding forms ===

    unshadowCase "application without shadowing unchanged"
      (apply (var "f") (int32 42))
      (apply (var "f") (int32 42)),

    unshadowCase "list of literals unchanged"
      (list [int32 1, int32 2, int32 3])
      (list [int32 1, int32 2, int32 3]),

    unshadowCase "nested record unchanged"
      (record (nm "Point") [(nm "x", int32 10), (nm "y", int32 20)])
      (record (nm "Point") [(nm "x", int32 10), (nm "y", int32 20)])]

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for variable analysis and manipulation" $
    supergroup "variables" [
      freeVariablesGroup,
      normalizeTypeVariablesGroup,
      unshadowVariablesGroup]
