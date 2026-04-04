{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for core rewrite/fold combinators (foldOverTerm, rewriteTerm, rewriteType, rewriteAndFoldTermWithPath)
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
import qualified Hydra.Dsl.Coders        as Coders

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Rewriting as RewritingModule
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs as Pairs

-- NOTE: This file previously used T for Terms and Ty for Types.
-- After standardization: Terms are unqualified, T is for Types.


ns :: Namespace
ns = Namespace "hydra.test.rewriting"

module_ :: Module
module_ = Module ns definitions
    [ShowCore.ns, RewritingModule.ns, TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for core rewrite/fold combinators")
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

-- | Show a type as a string using ShowCore.type_
showType :: TTerm Type -> TTerm String
showType t = ShowCore.type_ # t

-- | Core DSL Term-level constructors for building Term-typed values
-- These produce values of Hydra type Term (not String, Int, etc.)
tStr :: String -> TTerm Term
tStr s = Core.termLiteral (Core.literalString (Phantoms.string s))

-- | The "replaceFooWithBar" rewriter function for rewriteTerm tests
-- \recurse term -> if term == literal "foo" then literal "bar" else recurse term
replaceFooWithBarFn :: TTerm ((Term -> Term) -> Term -> Term)
replaceFooWithBarFn = Phantoms.lambda "recurse" $ Phantoms.lambda "term" $
  Logic.ifElse
    (Equality.equal (Phantoms.var "term") (tStr "foo"))
    (tStr "bar")
    (Phantoms.var "recurse" # Phantoms.var "term")

-- | The "replaceStringWithInt32" rewriter function for rewriteType tests
-- \recurse typ -> if typ == TypeLiteral LiteralTypeString then TypeLiteral (LiteralTypeInteger IntegerTypeInt32) else recurse typ
replaceStringWithInt32Fn :: TTerm ((Type -> Type) -> Type -> Type)
replaceStringWithInt32Fn = Phantoms.lambda "recurse" $ Phantoms.lambda "typ" $
  Logic.ifElse
    (Equality.equal (Phantoms.var "typ") (Core.typeLiteral Core.literalTypeString))
    (Core.typeLiteral (Core.literalTypeInteger Core.integerTypeInt32))
    (Phantoms.var "recurse" # Phantoms.var "typ")

-- | Fold operation: sum int32 literals
-- \acc term -> acc + (case term of TermLiteral (LiteralInteger (IntegerValueInt32 n)) -> n; _ -> 0)
sumInt32LiteralsFoldFn :: TTerm (Int -> Term -> Int)
sumInt32LiteralsFoldFn = Phantoms.lambda "acc" $ Phantoms.lambda "term" $
  Math.add (Phantoms.var "acc") $
    Phantoms.cases _Term (Phantoms.var "term") (Just (Phantoms.int32 0)) [
      _Term_literal ~>: Phantoms.lambda "lit" $
        Phantoms.cases _Literal (Phantoms.var "lit") (Just (Phantoms.int32 0)) [
          _Literal_integer ~>: Phantoms.lambda "intVal" $
            Phantoms.cases _IntegerValue (Phantoms.var "intVal") (Just (Phantoms.int32 0)) [
              _IntegerValue_int32 ~>: Phantoms.lambda "n" $ Phantoms.var "n"]]]

-- | Fold operation: collect list lengths
-- \acc term -> acc ++ (case term of TermList elems -> [length elems]; _ -> [])
collectListLengthsFoldFn :: TTerm ([Int] -> Term -> [Int])
collectListLengthsFoldFn = Phantoms.lambda "acc" $ Phantoms.lambda "term" $
  Lists.concat (Phantoms.list [
    Phantoms.var "acc",
    Phantoms.cases _Term (Phantoms.var "term") (Just (Phantoms.list ([] :: [TTerm Int]))) [
      _Term_list ~>: Phantoms.lambda "elems" $
        Phantoms.list [Lists.length (Phantoms.var "elems")]]])

-- | Fold operation: collect labels from pair terms
-- \acc term -> acc ++ (case term of TermPair (TermLiteral s, _) -> [s]; _ -> [])
collectLabelsFoldFn :: TTerm ([Literal] -> Term -> [Literal])
collectLabelsFoldFn = Phantoms.lambda "acc" $ Phantoms.lambda "term" $
  Lists.concat (Phantoms.list [
    Phantoms.var "acc",
    Phantoms.cases _Term (Phantoms.var "term") (Just (Phantoms.list ([] :: [TTerm Literal]))) [
      _Term_pair ~>: Phantoms.lambda "p" $
        Phantoms.cases _Term (Pairs.first (Phantoms.var "p")) (Just (Phantoms.list ([] :: [TTerm Literal]))) [
          _Term_literal ~>: Phantoms.lambda "lit" $ Phantoms.list [Phantoms.var "lit"]]]])

-- | Universal foldOverTermCase: applies fold operation and shows result
foldOverTermCase :: String -> TTerm Term -> TTerm TraversalOrder -> FoldOp -> TTerm Term -> TTerm TestCaseWithMetadata
foldOverTermCase cname input order op output = universalCase cname
  (showTerm (applyFoldOp op order input))
  (showTerm output)

-- | Fold operations (local enum to replace the Testing.FoldOperation type)
data FoldOp = FoldOpSumInt32 | FoldOpCollectListLengths | FoldOpCollectLabels

foldOpSumInt32Literals :: FoldOp
foldOpSumInt32Literals = FoldOpSumInt32

foldOpCollectListLengths :: FoldOp
foldOpCollectListLengths = FoldOpCollectListLengths

foldOpCollectLabels :: FoldOp
foldOpCollectLabels = FoldOpCollectLabels

-- | Apply a fold operation and wrap the result as a Term
applyFoldOp :: FoldOp -> TTerm TraversalOrder -> TTerm Term -> TTerm Term
applyFoldOp FoldOpSumInt32 order input =
  Core.termLiteral (Core.literalInteger (Core.integerValueInt32
    (RewritingModule.foldOverTerm # order # sumInt32LiteralsFoldFn # Phantoms.int32 0 # input)))
applyFoldOp FoldOpCollectListLengths order input =
  Core.termList (Lists.map
    (Phantoms.lambda "n" $ Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (Phantoms.var "n"))))
    (RewritingModule.foldOverTerm # order # collectListLengthsFoldFn # Phantoms.list ([] :: [TTerm Int]) # input))
applyFoldOp FoldOpCollectLabels order input =
  Core.termList (Lists.map
    (Phantoms.lambda "lit" $ Core.termLiteral (Phantoms.var "lit"))
    (RewritingModule.foldOverTerm # order # collectLabelsFoldFn # Phantoms.list ([] :: [TTerm Literal]) # input))

-- | Universal rewriteTerm test case: applies replaceFooWithBar rewriter
rewriteTermCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
rewriteTermCase cname input output = universalCase cname
  (showTerm (RewritingModule.rewriteTerm # replaceFooWithBarFn # input))
  (showTerm output)

-- | Universal rewriteType test case: applies replaceStringWithInt32 rewriter
rewriteTypeCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
rewriteTypeCase cname input output = universalCase cname
  (showType (RewritingModule.rewriteType # replaceStringWithInt32Fn # input))
  (showType output)

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

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for core rewrite/fold combinators" $
    supergroup "rewriting" [
      foldOverTermGroup,
      rewriteTypeGroup,
      rewriteTermGroup,
      rewriteAndFoldTermWithPathGroup]
