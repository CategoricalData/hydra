module Hydra.RewritingSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Rewriting
import Hydra.Impl.Haskell.Dsl.Terms

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
          (list [int32 42, apply (lambda "x" $ variable "x") (int32 10)] :: Term Meta))
        52

    H.it "Check that traversal order is respected" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre listLengths []
          (list [list [string "foo", string "bar"], apply (lambda "x" $ variable "x") (list [string "quux"])] :: Term Meta))
        [1, 2, 2]
      H.shouldBe
        (foldOverTerm TraversalOrderPost listLengths []
          (list [list [string "foo", string "bar"], apply (lambda "x" $ variable "x") (list [string "quux"])] :: Term Meta))
        [2, 1, 2]
  where
    addInt32s sum term = case termExpr term of
      TermExprLiteral (LiteralInteger (IntegerValueInt32 i)) -> sum + i
      _ -> sum
    listLengths l term = case termExpr term of
      TermExprList els -> L.length els:l
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
        (freeVariablesInTerm (string "foo" :: Term ()))
        S.empty
      H.shouldBe
        (freeVariablesInTerm (variable "x" :: Term ()))
        (S.fromList [Variable "x"])
      H.shouldBe
        (freeVariablesInTerm (list [variable "x", apply (lambda "y" $ variable "y") (int32 42)] :: Term ()))
        (S.fromList [Variable "x"])
      H.shouldBe
        (freeVariablesInTerm (list [variable "x", apply (lambda "y" $ variable "y") (variable "y")] :: Term ()))
        (S.fromList [Variable "x", Variable "y"])
        
--testReplaceFreeTypeVariable :: H.SpecWith ()
--testReplaceFreeTypeVariable = do
--  H.describe "Test replace free type variables" $ do
--    
--    H.it "Check that variable types are replaced" $ do
--      H.shouldBe
--        (replaceFreeTypeVariable (TypeVariable "v1") Types.string $ Types.variable "v")
--        ()
        
testReplaceTerm :: H.SpecWith ()
testReplaceTerm = do
    H.describe "Test term replacement" $ do

      H.it "Check that the correct subterms are replaced" $ do
        H.shouldBe
          (rewriteTerm replaceInts keepMeta
            (int32 42))
          (int64 42 :: Term Meta)
        H.shouldBe
          (rewriteTerm replaceInts keepMeta
            (list [int32 42, apply (lambda "x" $ variable "x") (int32 137)]))
          (list [int64 42, apply (lambda "x" $ variable "x") (int64 137)] :: Term Meta)

      H.it "Check that traversal order is respected" $ do
        H.shouldBe
          (rewriteTerm replaceListsPre keepMeta
            (list [list [list []]]))
          (list [list []] :: Term Meta)
        H.shouldBe
          (rewriteTerm replaceListsPost keepMeta
            (list [list [list []]]))
          (list [] :: Term Meta)

      H.it "Check that metadata is replace recursively" $ do
        H.shouldBe
          (rewriteTerm keepTerm replaceMeta (list [Term (TermExprLiteral $ LiteralString "foo") 42] :: Term Int))
          (Term (TermExprList [Term (TermExprLiteral $ LiteralString "foo") "42"]) "0")
  where
    keepTerm recurse term = recurse term

    keepMeta = id

    replaceInts recurse term = case termExpr term2 of
        TermExprLiteral (LiteralInteger (IntegerValueInt32 v)) -> int64 $ fromIntegral v
        _ -> term2
      where
        term2 = recurse term

    replaceLists term = case termExpr term of
      TermExprList (h:_) -> case termExpr h of
        TermExprList [] -> list []
        _ -> term
      _ -> term

    replaceListsPre recurse = recurse . replaceLists

    replaceListsPost recurse = replaceLists . recurse

    replaceMeta i = show i

testSimplifyTerm :: H.SpecWith ()
testSimplifyTerm = do
  H.describe "Test term simplifation (optimization)" $ do

    H.it "Check that 'const' applications are simplified" $ do
      H.shouldBe
        (simplifyTerm (apply (lambda "x" (string "foo")) (int32 42)))
        (string "foo" :: Term Meta)
      H.shouldBe
        (simplifyTerm (apply (lambda "x" $ list [variable "x", variable "x"]) (variable "y")))
        (list [variable "y", variable "y"] :: Term Meta)
      H.shouldBe
        (simplifyTerm (apply (lambda "x" $ string "foo") (variable "y")))
        (string "foo" :: Term Meta)
      H.shouldBe
        (simplifyTerm (apply (lambda "x"
          (apply (lambda "a" (list [string "foo", variable "a"])) (variable "x"))) (variable "y")))
        (list [string "foo", variable "y"] :: Term Meta)
        
testStripMeta :: H.SpecWith ()
testStripMeta = do
  H.describe "Test stripping metadata from terms" $ do

    H.it "Strip typ annotations" $ do
      QC.property $ \(TypedTerm typ term) -> do
        H.shouldBe
          (contextTypeOf testContext $ termMeta term)
          (pure Nothing)
        H.shouldBe
          (contextTypeOf testContext $ termMeta $ withType testContext typ term)
          (pure $ Just typ)
        H.shouldBe
          (contextTypeOf testContext $ termMeta $ stripMeta $ withType testContext typ term)
          (pure Nothing)

spec :: H.Spec
spec = do
  testFoldOverTerm
  testFreeVariablesInTerm
--  testReplaceFreeTypeVariable
  testReplaceTerm
  testSimplifyTerm
  testStripMeta
