{-# LANGUAGE OverloadedStrings #-}

{-
Test.Hspec.hspec Hydra.RewritingSpec.spec
-}
module Hydra.RewritingSpec where

import Hydra.Kernel
import Hydra.Flows
import Hydra.Tools.Monads
import Hydra.Dsl.Terms as Terms
import Hydra.Lib.Io
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data Quux a = QuuxUnit | QuuxValue a | QuuxPair (Quux a) (Quux a) deriving (Eq, Ord, Show)

fsubQuux :: (a -> b) -> (Quux a -> Quux b) -> Quux a -> Quux b
fsubQuux mf recurse q = case q of
  QuuxUnit -> QuuxUnit
  QuuxValue x -> QuuxValue $ mf x
  QuuxPair left right -> QuuxPair (recurse left) (recurse right)

rewriteQuux :: (a -> b) -> ((Quux a -> Quux b) -> Quux a -> Quux b) -> Quux a -> Quux b
rewriteQuux mf f = rewrite (fsubQuux mf) f

myQuuxRewriter :: Quux String -> Quux Int
myQuuxRewriter = rewriteQuux L.length $ \fsub q -> fsub $ case q of
  QuuxPair left right -> QuuxPair QuuxUnit right
  _ -> q

checkFoldOverTerm :: H.SpecWith ()
checkFoldOverTerm = do
  H.describe "Test foldOverTerm" $ do
    H.describe "Pre-order" $ do
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPre node1)
          ["a"]
      H.it "test #2" $
        H.shouldBe
          (traverse TraversalOrderPre node2)
          ["a", "b", "c", "d"]
    H.describe "Post-order" $ do
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPost node1)
          ["a"]
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPost node2)
          ["b", "d", "c", "a"]
  where
    node label children = Terms.pair (Terms.string label) (Terms.list children)
    labelOf term = case term of
      TermProduct [TermLiteral (LiteralString label), _] -> Just label
      _ -> Nothing
    traverse :: TraversalOrder -> Term -> [String]
    traverse order = Y.catMaybes . foldOverTerm order (\l t -> l ++ [labelOf t]) []
    node1 = node "a" []
    node2 = node "a" [node "b" [], node "c" [node "d" []]]

testExpandLambdas :: Graph -> H.SpecWith ()
testExpandLambdas g = do
  H.describe "Test expanding to (untyped) lambda terms" $ do

    H.describe "Try terms which do not expand" $ do
      H.it "test #1" $
        noChange (int32 42)
      H.it "test #2" $
        noChange (list ["foo", "bar"])
      H.it "test #3" $
        noChange (splitOn @@ "foo" @@ "bar")
      H.it "test #4" $
        noChange (lambda "x" $ lambda "y" $ splitOn @@ var "x" @@ var "y")
      H.it "test #5" $
        noChange (lambda "x" $ int32 42)

    H.describe "Try bare function terms" $ do
      H.it "test #1" $
        expandsTo
          toLower
          (lambda "v1" $ toLower @@ var "v1")
      H.it "test #2" $
        expandsTo
          splitOn
          (lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2")
      H.it "test #3" $
        expandsTo
          (splitOn @@ var "foo")
          (lambda "v1" $ splitOn @@ var "foo" @@ var "v1")
      H.it "test #4" $
        expandsTo
          (splitOn @@ var "foo" @@ var "bar")
          (splitOn @@ var "foo" @@ var "bar")
      H.it "test #5" $
        expandsTo
          (splitOn @@ var "foo" @@ var "bar" @@ var "baz")
          (splitOn @@ var "foo" @@ var "bar" @@ var "baz")
      H.it "test #6" $
        expandsTo
          (primitive _optionals_maybe @@ (int32 42) @@ length)
          -- Note two levels of lambda expansion
          (lambda "v1" $ (primitive _optionals_maybe @@ (int32 42) @@ (lambda "v1" $ length @@ var "v1")) @@ var "v1")
      H.it "test #7" $
        expandsTo
          (project (Name "Person") (Name "firstName"))
          (lambda "v1" $ ((project (Name "Person") (Name "firstName") @@ var "v1")))
      -- TODO: case statement

    H.describe "Try subterms within applications" $ do
      H.it "test #1" $
        expandsTo
          (splitOn @@ "bar")
          (lambda "v1" $ splitOn @@ "bar" @@ var "v1")
      H.it "test #2" $
        expandsTo
          (lambda "x" $ splitOn @@ var "x")
          (lambda "x" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")
      H.it "test #3" $
        expandsTo
          ((lambda "x" $ var "x") @@ length)
          (lambda "v1" $ length @@ var "v1")

    H.describe "Try let terms" $ do
      H.it "test #1" $
        noChange
          (lets ["foo">: int32 137] $ int32 42)
      H.it "test #2" $
        expandsTo
          (lets ["foo">: splitOn] $ var "foo")
          (lets ["foo">: lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2"] $ var "foo")

    H.describe "Try other subterms" $ do
      H.it "test #1" $
        expandsTo
          (list [lambda "x" $ list ["foo"], splitOn @@ "bar"])
          (list [lambda "x" $ list ["foo"], lambda "v1" $ splitOn @@ "bar" @@ var "v1"])

    H.it "Check that lambda expansion is idempotent" $ do
      QC.property $ \term -> do
        let once = expandLambdas g term
        let twice = expandLambdas g once
        H.shouldBe once twice
  where
    length = primitive $ Name "hydra.lib.strings.length"
    splitOn = primitive $ Name "hydra.lib.strings.splitOn"
    toLower = primitive $ Name "hydra.lib.strings.toLower"
    expandsTo termBefore termAfter = do
       let result = expandLambdas g termBefore
       H.shouldBe (show result) (show termAfter)
    noChange term = expandsTo term term

-- TODO: merge this into expandLambdas
testExpandTypedLambdas :: H.SpecWith ()
testExpandTypedLambdas = do
  H.describe "Test expanding to typed lambda terms" $ do

    H.describe "Try some terms which do not expand" $ do
      H.it "test #1" $
        noChange (int32 42)
      H.it "test #2" $
        noChange (list ["foo", "bar"])
      H.it "test #3" $
        noChange (splitOn @@ "foo" @@ "bar")
      H.it "test #4" $
        noChange (lambda "x" $ int32 42)

    H.describe "Expand bare function terms" $ do
      H.it "test #1" $
        expandsTo
          toLower
          (lambda "v1" $ toLower @@ var "v1")
      H.it "test #2" $
        expandsTo
          splitOn
          (lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2")
      H.it "test #3" $
        expandsTo
          (primitive _optionals_maybe @@ (int32 42) @@ length)
          -- Note two levels of lambda expansion
          (lambda "v1" $ (primitive _optionals_maybe @@ (int32 42) @@ (lambda "v1" $ length @@ var "v1")) @@ var "v1")
      H.it "test #4" $
        expandsTo
          (project (Name "Person") (Name "firstName"))
          (lambda "v1" $ ((project (Name "Person") (Name "firstName") @@ var "v2")))
      -- TODO: case statement

    H.describe "Expand subterms within applications" $ do
      H.it "test #1" $
        expandsTo
          (splitOn @@ "bar")
          (lambda "v1" $ splitOn @@ "bar" @@ var "v1")
      H.it "test #2" $
        expandsTo
          ((lambda "x" $ var "x") @@ length)
          ((lambda "x" $ var "x") @@ (lambda "v1" $ length @@ var "v1"))

    H.describe "Expand arbitrary subterms" $ do
      H.it "test #1" $
        expandsTo
          (list [lambda "x" $ list ["foo"], splitOn @@ "bar"])
          (list [lambda "x" $ list ["foo"], lambda "v1" $ splitOn @@ "bar" @@ var "v1"])

    H.it "Check that lambda expansion is idempotent" $ do
      QC.property $ \term -> do
        inf <- fromFlowIo testGraph $ inferTermType term
        let once = expandTypedLambdas term
        let twice = expandTypedLambdas once
        H.shouldBe once twice

  where
    length = primitive $ Name "hydra.lib.strings.length"
    splitOn = primitive $ Name "hydra.lib.strings.splitOn"
    toLower = primitive $ Name "hydra.lib.strings.toLower"
    expandsTo termBefore termAfter = do
--      result <- fromFlowIo testGraph $ expandLambdas termBefore
--      H.shouldBe result termAfter
       inf <- fromFlowIo testGraph $ inferTermType termBefore
       let result = expandTypedLambdas inf
       H.shouldBe (showTerm (removeTermAnnotations result)) (showTerm termAfter)
    noChange term = expandsTo term term

testFoldOverTerm :: H.SpecWith ()
testFoldOverTerm = do
  H.describe "Test folding over terms" $ do

    H.it "Try a simple fold" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre addInt32s 0
          (list [int32 42, (lambda "x" $ var "x") @@ int32 10]))
        52

    H.it "Check that traversal order is respected" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre listLengths []
          (list [list [string "foo", string "bar"], (lambda "x" $ var "x") @@ (list [string "quux"])]))
        [1, 2, 2]
      H.shouldBe
        (foldOverTerm TraversalOrderPost listLengths []
          (list [list [string "foo", string "bar"], (lambda "x" $ var "x") @@ (list [string "quux"])]))
        [2, 1, 2]
  where
    addInt32s sum term = case term of
      TermLiteral (LiteralInteger (IntegerValueInt32 i)) -> sum + i
      _ -> sum
    listLengths l term = case term of
      TermList els -> L.length els:l
      _ -> l

testFlattenLetTerms :: H.SpecWith ()
testFlattenLetTerms = do
  H.describe "Test flattening of 'let' terms" $ do

    H.it "Non-let terms are unaffected" $ do
      H.shouldBe
        (flattenLetTerms $ Terms.int32 42)
        (Terms.int32 42)
      H.shouldBe
        (flattenLetTerms $ Terms.list [Terms.string "foo"])
        (Terms.list [Terms.string "foo"])

    H.it "Non-nested let terms are unaffected" $
      H.shouldBe
        (flattenLetTerms letTerm1)
        (letTerm1)

    H.it "Nonrecursive, nested bindings are flattened" $
      H.shouldBe
        (flattenLetTerms letTerm2)
        (letTerm2_flattened)

    H.it "Multiple levels of nesting are flattened appropriately" $
      H.shouldBe
        (flattenLetTerms letTerm3)
        (letTerm3_flattened)
  where
    makeLet body pairs = TermLet $ Let (makeBinding <$> pairs) body
      where
        makeBinding (k, v) = LetBinding (Name k) v Nothing
    letTerm1 = makeLet (TermList [Terms.var "x", Terms.var "y"]) [
      ("x", Terms.int32 1),
      ("y", Terms.int32 2)]
    letTerm2 = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", letTerm1)]
    letTerm2_flattened = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", TermList [Terms.var "b_x", Terms.var "b_y"]),
      ("b_x", Terms.int32 1),
      ("b_y", Terms.int32 2)]
    letTerm3 = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", makeLet (TermList [Terms.var "x", Terms.var "y"]) [
        ("x", Terms.int32 1),
        ("y", makeLet (TermList [Terms.var "a", Terms.var "q"]) [
          ("p", Terms.int32 137),
          ("q", TermList [Terms.var "x", Terms.int32 5])])])]
    letTerm3_flattened = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", TermList [Terms.var "b_x", Terms.var "b_y"]),
      ("b_x", Terms.int32 1),
      ("b_y", TermList [Terms.var "a", Terms.var "b_y_q"]),
      ("b_y_p", Terms.int32 137),
      ("b_y_q", TermList [Terms.var "b_x", Terms.int32 5])]

testFreeVariablesInTerm :: H.SpecWith ()
testFreeVariablesInTerm = do
  H.describe "Test free variables" $ do

--    H.it "Generated terms never have free variables" $ do
--      QC.property $ \(TypedTerm term _) -> do
--        H.shouldBe
--          (freeVariablesInTerm (term))
--          S.empty

    H.it "Free variables in individual terms" $ do
      H.shouldBe
        (freeVariablesInTerm (string "foo"))
        S.empty
      H.shouldBe
        (freeVariablesInTerm (var "x"))
        (S.fromList [Name "x"])
      H.shouldBe
        (freeVariablesInTerm (list [var "x", (lambda "y" $ var "y") @@ int32 42]))
        (S.fromList [Name "x"])
      H.shouldBe
        (freeVariablesInTerm (list [var "x", (lambda "y" $ var "y") @@ var "y"]))
        (S.fromList [Name "x", Name "y"])

testNormalizeTypeVariablesInTerm :: H.SpecWith ()
testNormalizeTypeVariablesInTerm = do
    H.describe "No type variables" $ do
      H.it "test #1" $ noChange
        (int32 42)
      H.it "test #2" $ noChange
        (tlet (int32 42) [
          ("foo", Nothing, string "foo")])
      H.it "test #3" $ noChange
        (tlet (int32 42) [
          ("foo", Just tsString, string "foo")])
      H.it "test #4" $ noChange
        (withMonoFoo $ int32 42)

    H.describe "Only free type variables" $ do
      H.it "test #1" $ noChange
        (withPolyFoo $ int32 42)
      H.it "test #2" $ noChange
        (withMonoFoo const42)
      H.it "test #3" $ noChange
        (withPolyFoo const42)

    H.describe "Simple polymorphic let bindings" $ do
      H.it "test #1" $ changesTo
        (withIdBefore id42)
        (withIdAfter id42)

    H.describe "Rewriting of bindings does not affect environment" $ do
      H.it "test #1" $ changesTo
        (withIdBefore const42) -- Free variable "a" coincides with bound variable "a", but in a different branch.
        (withIdAfter const42)
      H.it "test #2" $ changesTo -- Same substitution in bindings and environment
        (withIdBefore (withIdBefore id42))
        (withIdAfter (withIdAfter id42))

    H.describe "Nested polymorphic let bindings" $ do
      H.it "Parent variable shadows child variable" $ changesTo
        (tlet id42 [
          ("id", Just faa, tlet (lambdaTyped "y" tA $ var "id2" @@ var "y") [
            ("id2", Just faa, lambdaTyped "x" tA $ var "x")])])
        (tlet id42 [
          ("id", Just ft0t0, tlet (lambdaTyped "y" t0 $ var "id2" @@ var "y") [
            ("id2", Just ft1t1, lambdaTyped "x" t1 $ var "x")])])
      H.it "No shadowing" $ changesTo
        (tlet id42 [
          ("id", Just faa, tlet (lambdaTyped "y" tA $ var "id2" @@ var "y") [
            ("id2", Just fbb, lambdaTyped "x" tB $ var "x")])])
        (tlet id42 [
          ("id", Just ft0t0, tlet (lambdaTyped "y" t0 $ var "id2" @@ var "y") [
            ("id2", Just ft1t1, lambdaTyped "x" t1 $ var "x")])])
      H.it "No shadowing, locally free type variable" $ changesTo
        (tlet (var "fun1" @@ string "foo" @@ int32 42) [
          ("fun1", Just (Types.poly ["a", "b"] $ Types.functionN [tA, tB, tPair tA tB]), lambdaTyped "x" tA $ lambdaTyped "y" tB $
            tlet (var "fun2" @@ var "x") [
              ("fun2", Just (Types.poly ["c"] $ tFun tC $ tPair tC tB), lambdaTyped "z" tC $ pair (var "z") (var "y"))])])
        (tlet (var "fun1" @@ string "foo" @@ int32 42) [
          ("fun1", Just (Types.poly ["t0", "t1"] $ Types.functionN [t0, t1, tPair t0 t1]), lambdaTyped "x" t0 $ lambdaTyped "y" t1 $
            tlet (var "fun2" @@ var "x") [
              ("fun2", Just (Types.poly ["t2"] $ tFun t2 $ tPair t2 t1), lambdaTyped "z" t2 $ pair (var "z") (var "y"))])])
  where
    changesTo term1 term2 = H.shouldBe (normalize term1) term2
    noChange term = H.shouldBe (normalize term) term
    normalize = normalizeTypeVariablesInTerm
    tlet env triples = TermLet $ Let (toBinding <$> triples) env
      where
        toBinding (key, mts, value) = LetBinding (Name key) value mts
    t0 = Types.var "t0"
    t1 = Types.var "t1"
    t2 = Types.var "t2"
    const42 = lambdaTyped "x" (Types.function tA tInt32) $ int32 42
    faa = Types.poly ["a"] $ tFun tA tA
    fbb = Types.poly ["b"] $ tFun tB tB
    ft0t0 = Types.poly ["t0"] $ tFun t0 t0
    ft1t1 = Types.poly ["t1"] $ tFun t1 t1
    id42 = var "id" @@ int32 42
    tsString = Types.mono Types.string
    tsA = Types.mono $ Types.var "a"
    withIdBefore term = tlet term [
      ("id", Just faa, lambda "x" $ var "x")]
    withIdAfter term = tlet term [
      ("id", Just ft0t0, lambda "x" $ var "x")]
    withMonoFoo term = tlet term [
      ("foo", Just tsString, string "foo")]
    withPolyFoo term = tlet term [
      ("foo", Just tsA, var "bar")]

testReplaceTerm :: H.SpecWith ()
testReplaceTerm = do
    H.describe "Test term replacement" $ do

      H.it "Check that the correct subterms are replaced" $ do
        H.shouldBe
          (rewriteTerm replaceInts
            (int32 42))
          (int64 42)
        H.shouldBe
          (rewriteTerm replaceInts
            (list [int32 42, (lambda "x" $ var "x") @@ int32 137]))
          (list [int64 42, (lambda "x" $ var "x") @@ int64 137])

      H.it "Check that traversal order is respected" $ do
        H.shouldBe
          (rewriteTerm replaceListsPre
            (list [list [list []]]))
          (list [list []])
        H.shouldBe
          (rewriteTerm replaceListsPost
            (list [list [list []]]))
          (list [])

--      H.it "Check that metadata is replace recursively" $ do
--        H.shouldBe
--          (rewriteTerm keepTerm replaceKv (list [annot 42 (string "foo")] Int))
--          (list [annot "42" (string "foo")])
  where
    keepTerm recurse term = recurse term

    replaceInts recurse term = case term2 of
        TermLiteral (LiteralInteger (IntegerValueInt32 v)) -> int64 $ fromIntegral v
        _ -> term2
      where
        term2 = recurse term

    replaceLists term = case term of
      TermList (h:_) -> case h of
        TermList [] -> list []
        _ -> term
      _ -> term

    replaceListsPre recurse = recurse . replaceLists

    replaceListsPost recurse = replaceLists . recurse

    replaceKv i = show i

testRewriteExampleType :: H.SpecWith ()
testRewriteExampleType = do
    H.describe "Test rewriting of a made-up recursive type" $ do

      H.it "Rewrite a hand-picked expression" $ do
        H.shouldBe
          quux2
          (myQuuxRewriter quux1)
  where
    quux1 = QuuxPair QuuxUnit (QuuxPair (QuuxValue "abc") (QuuxValue "12345"))
    quux2 = QuuxPair QuuxUnit (QuuxPair QuuxUnit (QuuxValue 5))

testSimplifyTerm :: H.SpecWith ()
testSimplifyTerm = do
  H.describe "Test term simplifation (optimization)" $ do

    H.it "Check that 'const' applications are simplified" $ do
      H.shouldBe
        (simplifyTerm $ (lambda "x" $ string "foo") @@ int32 42)
        (string "foo")
      H.shouldBe
        (simplifyTerm ((lambda "x" $ list [var "x", var "x"]) @@ var "y"))
        (list [var "y", var "y"])
      H.shouldBe
        (simplifyTerm ((lambda "x" $ string "foo") @@ var "y"))
        (string "foo")
      H.shouldBe
        (simplifyTerm ((lambda "x"
          ((lambda "a" (list [string "foo", var "a"])) @@ var "x")) @@ var "y"))
        (list [string "foo", var "y"])

--testStripAnnotations :: H.SpecWith ()
--testStripAnnotations = do
--  H.describe "Test stripping metadata from terms" $ do
--
--    H.it "Strip type annotations" $ do
--      QC.property $ \(TypedTerm term typ) -> do
--        shouldSucceedWith
--          (getTermType term)
--          Nothing
--        shouldSucceedWith
--          (getTermType $ withType typ term)
--          (Just typ)
--        shouldSucceedWith
--          (getTermType $ strip $ withType typ term)
--          Nothing

testTopologicalSortBindings :: H.SpecWith ()
testTopologicalSortBindings = do
    H.describe "Test topological sort of bindings" $ do

      H.it "Isolated bindings" $ do
        checkBindings
          [("a", string "foo"), ("b", string "bar")]
          [["b"], ["a"]]

      H.it "Single recursive binding" $ do
        checkBindings
          [("a", list [var "a"])]
          [["a"]]

      H.it "Mutually recursive bindings" $ do
        checkBindings
          [("a", list [var "b"]), ("b", list [var "a"])]
          [["a", "b"]]

      H.it "Mixed bindings" $ do
        checkBindings
          [("a", var "b"), ("b", list [var "a", var "c"]), ("c", string "foo"), ("d", string "bar")]
          [["d"], ["c"], ["a", "b"]]
  where
    checkBindings bindings expectedVars = H.shouldBe
        (topologicalSortBindings bindingMap)
        expected
      where
        bindingMap = M.mapKeys (\k -> Name k) $ M.fromList bindings
        expected = fmap (fmap (\k -> (Name k, Y.fromMaybe unit $ M.lookup (Name k) bindingMap))) expectedVars

--withType :: Graph -> Type -> Term -> Term
--withType typ = setTermType $ Just typ

spec :: H.Spec
spec = do
  checkFoldOverTerm
  testFoldOverTerm

  testExpandLambdas testGraph
--  testExpandTypedLambdas -- TODO: restore me / merge with testExpandLambdas
  testFlattenLetTerms
  testFreeVariablesInTerm
  testNormalizeTypeVariablesInTerm
  testReplaceTerm
  testRewriteExampleType
  testSimplifyTerm
--  testStripAnnotations -- TODO: restore me
  testTopologicalSortBindings
