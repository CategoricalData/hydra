module Hydra.RewritingSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Rewriting

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Set as S


testFoldOverData :: H.SpecWith ()
testFoldOverData = do
  H.describe "Test folding over terms" $ do

    H.it "Try a simple fold" $ do
      H.shouldBe
        (foldOverData TraversalOrderPre addInt32s 0
          (list [int32Value 42, apply (lambda "x" $ variable "x") (int32Value 10)] :: Data Meta))
        52

    H.it "Check that traversal order is respected" $ do
      H.shouldBe
        (foldOverData TraversalOrderPre listLengths []
          (list [list [stringValue "foo", stringValue "bar"], apply (lambda "x" $ variable "x") (list [stringValue "quux"])] :: Data Meta))
        [1, 2, 2]
      H.shouldBe
        (foldOverData TraversalOrderPost listLengths []
          (list [list [stringValue "foo", stringValue "bar"], apply (lambda "x" $ variable "x") (list [stringValue "quux"])] :: Data Meta))
        [2, 1, 2]
  where
    addInt32s sum term = case dataTerm term of
      DataTermLiteral (LiteralInteger (IntegerValueInt32 i)) -> sum + i
      _ -> sum
    listLengths l term = case dataTerm term of
      DataTermList els -> L.length els:l
      _ -> l

testFreeVariablesInData :: H.SpecWith ()
testFreeVariablesInData = do
  H.describe "Test free variables" $ do

    H.it "Generated terms never have free variables" $ do
      QC.property $ \(TypedData _ term) -> do
        H.shouldBe
          (freeVariablesInData (term :: Data ()))
          S.empty

    H.it "Free variables in individual terms" $ do
      H.shouldBe
        (freeVariablesInData (stringValue "foo" :: Data ()))
        S.empty
      H.shouldBe
        (freeVariablesInData (variable "x" :: Data ()))
        (S.fromList [Variable "x"])
      H.shouldBe
        (freeVariablesInData (list [variable "x", apply (lambda "y" $ variable "y") (int32Value 42)] :: Data ()))
        (S.fromList [Variable "x"])
      H.shouldBe
        (freeVariablesInData (list [variable "x", apply (lambda "y" $ variable "y") (variable "y")] :: Data ()))
        (S.fromList [Variable "x", Variable "y"])

testReplaceData :: H.SpecWith ()
testReplaceData = do
    H.describe "Test term replacement" $ do

      H.it "Check that the correct subterms are replaced" $ do
        H.shouldBe
          (rewriteData replaceInts keepMeta
            (int32Value 42))
          (int64Value 42 :: Data Meta)
        H.shouldBe
          (rewriteData replaceInts keepMeta
            (list [int32Value 42, apply (lambda "x" $ variable "x") (int32Value 137)]))
          (list [int64Value 42, apply (lambda "x" $ variable "x") (int64Value 137)] :: Data Meta)

      H.it "Check that traversal order is respected" $ do
        H.shouldBe
          (rewriteData replaceListsPre keepMeta
            (list [list [list []]]))
          (list [list []] :: Data Meta)
        H.shouldBe
          (rewriteData replaceListsPost keepMeta
            (list [list [list []]]))
          (list [] :: Data Meta)

      H.it "Check that metadata is replace recursively" $ do
        H.shouldBe
          (rewriteData keepData replaceMeta (list [Data (DataTermLiteral $ LiteralString "foo") 42] :: Data Int))
          (Data (DataTermList [Data (DataTermLiteral $ LiteralString "foo") "42"]) "0")
  where
    keepData recurse term = recurse term

    keepMeta = id

    replaceInts recurse term = case dataTerm term2 of
        DataTermLiteral (LiteralInteger (IntegerValueInt32 v)) -> int64Value $ fromIntegral v
        _ -> term2
      where
        term2 = recurse term

    replaceLists term = case dataTerm term of
      DataTermList (h:_) -> case dataTerm h of
        DataTermList [] -> list []
        _ -> term
      _ -> term

    replaceListsPre recurse = recurse . replaceLists

    replaceListsPost recurse = replaceLists . recurse

    replaceMeta i = show i

testSimplifyData :: H.SpecWith ()
testSimplifyData = do
  H.describe "Test term simplifation (optimization)" $ do

    H.it "Check that 'const' applications are simplified" $ do
      H.shouldBe
        (simplifyData (apply (lambda "x" (stringValue "foo")) (int32Value 42)))
        (stringValue "foo" :: Data Meta)

testStripMeta :: H.SpecWith ()
testStripMeta = do
  H.describe "Test stripping metadata from terms" $ do

    H.it "Strip typ annotations" $ do
      QC.property $ \(TypedData typ term) -> do
        H.shouldBe
          (contextTypeOf testContext $ dataMeta term)
          (pure Nothing)
        H.shouldBe
          (contextTypeOf testContext $ dataMeta $ withType testContext typ term)
          (pure $ Just typ)
        H.shouldBe
          (contextTypeOf testContext $ dataMeta $ stripMeta $ withType testContext typ term)
          (pure Nothing)

spec :: H.Spec
spec = do
  testFoldOverData
  testFreeVariablesInData
  testReplaceData
  testSimplifyData
  testStripMeta
