module Hydra.Prototyping.RewritingSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Prototyping.Rewriting

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Set as S


testFoldOverTerm :: H.SpecWith ()
testFoldOverTerm = do
  H.describe "Test folding over terms" $ do
    
    H.it "Try a simple fold" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre addInt32s 0
          (list [int32Value 42, apply (lambda "x" $ variable "x") (int32Value 10)] :: Term Meta))
        52
        
    H.it "Check that traversal order is respected" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre listLengths []
          (list [list [stringValue "foo", stringValue "bar"], apply (lambda "x" $ variable "x") (list [stringValue "quux"])] :: Term Meta))
        [1, 2, 2]
      H.shouldBe
        (foldOverTerm TraversalOrderPost listLengths []
          (list [list [stringValue "foo", stringValue "bar"], apply (lambda "x" $ variable "x") (list [stringValue "quux"])] :: Term Meta))
        [2, 1, 2]
  where
    addInt32s sum term = case termData term of
      ExpressionLiteral (LiteralInteger (IntegerValueInt32 i)) -> sum + i
      _ -> sum
    listLengths l term = case termData term of
      ExpressionList els -> L.length els:l
      _ -> l

testFreeVariablesInTerm :: H.SpecWith ()
testFreeVariablesInTerm = do
  H.describe "Test free variables" $ do

    H.it "Generated terms never have free variables" $ do
      QC.property $ \(TypedTerm _ term) -> do
        H.shouldBe
          (freeVariablesInTerm (term :: Term ()))
          S.empty

    H.it "Free variables in individual terms" $ do
      H.shouldBe
        (freeVariablesInTerm (stringValue "foo" :: Term ()))
        S.empty
      H.shouldBe
        (freeVariablesInTerm (variable "x" :: Term ()))
        (S.fromList ["x"])
      H.shouldBe
        (freeVariablesInTerm (list [variable "x", apply (lambda "y" $ variable "y") (int32Value 42)] :: Term ()))
        (S.fromList ["x"])
      H.shouldBe
        (freeVariablesInTerm (list [variable "x", apply (lambda "y" $ variable "y") (variable "y")] :: Term ()))
        (S.fromList ["x", "y"])

testReplaceTerm :: H.SpecWith ()
testReplaceTerm = do
    H.describe "Test term replacement" $ do

      H.it "Check that the correct subterms are replaced" $ do
        H.shouldBe
          (replaceTerm TraversalOrderPre replaceInts
            (int32Value 42))
          (int64Value 42 :: Term Meta)
        H.shouldBe
          (replaceTerm TraversalOrderPre replaceInts
            (list [int32Value 42, apply (lambda "x" $ variable "x") (int32Value 137)]))
          (list [int64Value 42, apply (lambda "x" $ variable "x") (int64Value 137)] :: Term Meta)

      H.it "Check that traversal order is respected" $ do
        H.shouldBe
          (replaceTerm TraversalOrderPre replaceLists
            (list [list [list []]]))
          (list [list []] :: Term Meta)
        H.shouldBe
          (replaceTerm TraversalOrderPost replaceLists
            (list [list [list []]]))
          (list [] :: Term Meta)
  where
    replaceLists term = Just $ case termData term of
      ExpressionList (h:_) -> case termData h of
        ExpressionList [] -> list []
        _ -> term
      _ -> term
    replaceInts term = Just $ case termData term of
      ExpressionLiteral (LiteralInteger (IntegerValueInt32 v)) -> int64Value $ fromIntegral v
      _ -> term

testSimplifyTerm :: H.SpecWith ()
testSimplifyTerm = do
  H.describe "Test term simplifation (optimization)" $ do

    H.it "Check that 'const' applications are simplified" $ do
      H.shouldBe
        (simplifyTerm (apply (lambda "x" (stringValue "foo")) (int32Value 42)))
        (stringValue "foo" :: Term Meta)

testStripMeta :: H.SpecWith ()
testStripMeta = do
  H.describe "Test stripping metadata from terms" $ do
    
    H.it "Strip typ annotations" $ do
      QC.property $ \(TypedTerm typ term) -> do
        H.shouldBe
          (contextTypeOf testContext $ termMeta term)
          Nothing                    
        H.shouldBe
          (contextTypeOf testContext $ termMeta $ withType testContext typ term)
          (Just typ)
        H.shouldBe
          (contextTypeOf testContext $ termMeta $ stripMeta $ withType testContext typ term)
          Nothing

spec :: H.Spec
spec = do
  testFoldOverTerm
  testFreeVariablesInTerm
  testReplaceTerm
  testSimplifyTerm
  testStripMeta
