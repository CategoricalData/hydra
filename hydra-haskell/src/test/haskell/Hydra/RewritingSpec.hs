{-# LANGUAGE OverloadedStrings #-}

module Hydra.RewritingSpec where

import Hydra.Kernel
import Hydra.Flows
import Hydra.Dsl.Terms as Terms
import Hydra.Lib.Io
import qualified Hydra.Dsl.Types as Types

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


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

testExpandLambdas :: H.SpecWith ()
testExpandLambdas = do
  H.describe "Test expanding to lambda terms" $ do

    H.it "Try some terms which do not expand" $ do
      noChange (int32 42)
      noChange (list ["foo", "bar"])
      noChange
        (splitOn @@ "foo" @@ "bar")
      noChange
        (lambda "x" $ int32 42)
      noChange
        (typed Types.int32 $ int32 42)

    H.it "Expand bare function terms" $ do
      expandsTo
        toLower
        (lambda "v1" $ toLower @@ var "v1")
      expandsTo
        splitOn
        (lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2")
      expandsTo
        (matchOpt (int32 42) length)
        -- Note two levels of lambda expansion
        (lambda "v1" $ (matchOpt (int32 42) $ lambda "v1" $ length @@ var "v1") @@ var "v1")

    H.it "Expand subterms within applications" $ do
      expandsTo
        (splitOn @@ "bar")
        (lambda "v1" $ splitOn @@ "bar" @@ var "v1")
      expandsTo
        ((lambda "x" $ var "x") @@ length)
        ((lambda "x" $ var "x") @@ (lambda "v1" $ length @@ var "v1"))

    H.it "Expand arbitrary subterms" $ do
      expandsTo
        (list [lambda "x" $ list ["foo"], splitOn @@ "bar"])
        (list [lambda "x" $ list ["foo"], lambda "v1" $ splitOn @@ "bar" @@ var "v1"])

    H.it "Check that lambda expansion is idempotent" $ do
      QC.property $ \term -> do
        inf <- fromFlowIo testGraph $ annotateTypedTerms term
        let once = expandTypedLambdas term
        let twice = expandTypedLambdas once
        H.shouldBe once twice

  where
    length = primitive $ Name "hydra/lib/strings.length"
    splitOn = primitive $ Name "hydra/lib/strings.splitOn"
    toLower = primitive $ Name "hydra/lib/strings.toLower"
    expandsTo termBefore termAfter = do
--      result <- fromFlowIo testGraph $ expandLambdas termBefore
--      H.shouldBe result termAfter
       inf <- fromFlowIo testGraph $ annotateTypedTerms termBefore
       let result = expandTypedLambdas inf
       H.shouldBe (showTerm (removeTermAnnotations result)) (showTerm termAfter)

    noChange term = expandsTo term term

    app = lambda "a" $ project testTypePersonName (Name "firstName") @@ var "a"

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
    makeLet body pairs = TermLet $ Let (M.fromList (makePair <$> pairs)) body
      where
        makePair (k, v) = (Name k, v)
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

--testReplaceFreeName :: H.SpecWith ()
--testReplaceFreeName = do
--  H.describe "Test replace free type variables" $ do
--
--    H.it "Check that variable types are replaced" $ do
--      H.shouldBe
--        (replaceFreeName (Name "v1") Types.string $ Types.var "v")
--        ()

testReplaceTerm :: H.SpecWith ()
testReplaceTerm = do
    H.describe "Test term replacement" $ do

      H.it "Check that the correct subterms are replaced" $ do
        H.shouldBe
          (rewriteTerm replaceInts keepKv
            (int32 42))
          (int64 42)
        H.shouldBe
          (rewriteTerm replaceInts keepKv
            (list [int32 42, (lambda "x" $ var "x") @@ int32 137]))
          (list [int64 42, (lambda "x" $ var "x") @@ int64 137])

      H.it "Check that traversal order is respected" $ do
        H.shouldBe
          (rewriteTerm replaceListsPre keepKv
            (list [list [list []]]))
          (list [list []])
        H.shouldBe
          (rewriteTerm replaceListsPost keepKv
            (list [list [list []]]))
          (list [])

--      H.it "Check that metadata is replace recursively" $ do
--        H.shouldBe
--          (rewriteTerm keepTerm replaceKv (list [annot 42 (string "foo")] Int))
--          (list [annot "42" (string "foo")])
  where
    keepTerm recurse term = recurse term

    keepKv = id

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
--
--withType :: Graph -> Type -> Term -> Term
--withType typ = setTermType $ Just typ

spec :: H.Spec
spec = do
--   testExpandLambdas -- TODO: restore me
  testFoldOverTerm
  testFlattenLetTerms
  testFreeVariablesInTerm
--  testReplaceFreeName
  testReplaceTerm
  testRewriteExampleType
  testSimplifyTerm
--  testStripAnnotations -- TODO: restore me
