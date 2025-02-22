{-# LANGUAGE OverloadedStrings #-}

module Hydra.Inference.InferenceSpec where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.Staging.Inference
import Hydra.TestUtils
import Hydra.TestData
import qualified Hydra.Dsl.Expect as Expect
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Lib.Logic as Logic
import qualified Hydra.Dsl.Lib.Math as Math

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad


checkType :: Term -> Type -> H.Expectation
checkType term typ = typeAnn term `H.shouldBe` typ
  where
    typeAnn (TermTyped (TypedTerm _ typ)) = typ

expectMonotype :: Term -> Type -> H.Expectation
expectMonotype term = expectPolytype term []

expectPolytype :: Term-> [String] -> Type -> H.Expectation
expectPolytype term vars typ = do
    shouldSucceedWith
      (inferTypeScheme term)
      (TypeScheme (Name <$> vars) typ)

expectTypeAnnotation :: (Term -> Flow (Graph) (Term)) -> Term -> Type -> H.Expectation
expectTypeAnnotation path term etyp = shouldSucceedWith atyp etyp
  where
   atyp = do
     iterm <- inferTermType term
     selected <- path iterm
     case selected of
       TermTyped (TypedTerm _ typ) -> return typ
       _ -> fail $ "no type annotation"

checkLiterals :: H.SpecWith ()
checkLiterals = H.describe "Check arbitrary literals" $ do

    H.it "Verify that type inference preserves the literal to literal type mapping" $
      QC.property $ \l -> expectMonotype
        (TermLiteral l)
        (Types.literal $ literalType l)

checkTypeAnnotations :: H.SpecWith ()
checkTypeAnnotations = H.describe "Check that type annotations are added to terms and subterms" $ do

    H.it "Check literals" $
      QC.property $ \l -> do
        let term = TermLiteral l
        let term1 = fromFlow (TermLiteral $ LiteralString "no term") testGraph (fst <$> inferTypeAndConstraints term)
        checkType term1 (Types.literal $ literalType l)

    H.it "Check lists of literals" $
      QC.property $ \l -> do
        let term = TermList [TermLiteral l]
        let term1 = fromFlow (TermLiteral $ LiteralString "no term") testGraph (fst <$> inferTypeAndConstraints term)
        checkType term1 (Types.list $ Types.literal $ literalType l)
        let (TermTyped (TypedTerm (TermList [term2]) _)) = term1
        checkType term2 (Types.literal $ literalType l)

checkSubtermAnnotations :: H.SpecWith ()
checkSubtermAnnotations = H.describe "Check additional subterm annotations" $ do

    H.it "Check literals" $
      expectTypeAnnotation pure
        (string "foo")
        (Types.string)

    H.describe "Check monotyped lists" $ do
      H.it "test #1" $
        expectTypeAnnotation pure
          (list [string "foo"])
          (Types.list Types.string)
      H.it "test #2" $
        expectTypeAnnotation Expect.listHead
          (list [string "foo"])
          Types.string

    H.describe "Check monotyped lists within lambdas" $ do
      H.it "test #1" $
        expectTypeAnnotation pure
          (lambda "x" $ list [var "x", string "foo"])
          (Types.function Types.string (Types.list Types.string))
      H.it "test #2" $
        expectTypeAnnotation (Expect.lambdaBody >=> Expect.listHead)
          (lambda "x" $ list [var "x", string "foo"])
          Types.string

    H.describe "Check injections" $ do
      H.it "test #1" $
        expectTypeAnnotation pure
          (inject testTypeTimestampName $ Field (Name "date") $ string "2023-05-11")
          testTypeTimestamp
      H.it "test #2" $
        expectTypeAnnotation pure
          (lambda "ignored" $ (inject testTypeTimestampName $ Field (Name "date") $ string "2023-05-11"))
          (Types.function (Types.var "t0") testTypeTimestamp)

    H.it "Check projections" $ do
      expectTypeAnnotation pure
        (project testTypePersonName $ Name "firstName")
        (Types.function testTypePerson Types.string)

    H.describe "Check case statements" $ do
      H.it "test #1" $ do
        expectTypeAnnotation pure
          (match testTypeNumberName (Just $ string "it's something else") [
            Field (Name "int") $ constant $ string "it's an integer"])
          (Types.function testTypeNumber Types.string)
      H.describe "test #2" $ do
        let  testCase = match testTypeNumberName Nothing [
                          Field (Name "int") $ constant $ string "it's an integer",
                          Field (Name "float") $ constant $ string "it's a float"]
        H.it "case #1" $
          expectTypeAnnotation pure testCase
            (Types.function testTypeNumber Types.string)
        H.it "case #2" $
          expectTypeAnnotation (Expect.casesCase testTypeNumberName "int" >=> (pure . fieldTerm)) testCase
            (Types.function Types.int32 Types.string)

    H.describe "Check optional eliminations" $ do
      H.describe "test #1" $ do
        let testCase = matchOpt
                         (string "nothing")
                         (lambda "ignored" $ string "just")
        H.it "case #1" $
          expectTypeAnnotation pure testCase
            (Types.function (Types.optional $ Types.var "t2") Types.string)
        H.it "case #2" $
          expectTypeAnnotation Expect.optCasesNothing testCase
            Types.string
        H.it "case #3" $
          expectTypeAnnotation Expect.optCasesJust testCase
            (Types.function (Types.var "t2") Types.string)
      H.describe "test #2" $ do
        let testCase = lambda "getOpt" $ lambda "x" $
                         (matchOpt
                           (string "nothing")
                           (lambda "t2" $ string "just")) @@ (var "getOpt" @@ var "x")
        let getOptType = (Types.function (Types.var "t1") (Types.optional $ Types.var "t4"))
        let constStringType = Types.function (Types.var "t1") Types.string
        H.it "case #1" $
          expectTypeAnnotation pure testCase
            (Types.function getOptType constStringType)
        H.it "case #2" $
          expectTypeAnnotation Expect.lambdaBody testCase
            constStringType

    H.describe "Check unannotated 'let' terms" $ do
      H.describe "test #1" $ do
        let testCase = lambda "i" $
                         (Terms.primitive _strings_cat @@ list [string "foo", var "i", string "bar"])
                         `with` [
                           "foo">: string "FOO",
                           "bar">: string "BAR"]
        H.it "case #1" $
          expectTypeAnnotation pure testCase
            (Types.function Types.string Types.string)
        H.it "case #2" $
          expectTypeAnnotation Expect.lambdaBody testCase
            Types.string
      H.describe "test #2" $ do
        let testCase = lambda "original" $
                         var "alias" `with` [
                           "alias">: var "original"]
        H.it "case #1" $
          expectTypeAnnotation pure testCase
            (Types.function (Types.var "t0") (Types.var "t0"))
        H.it "case #2" $
          expectTypeAnnotation Expect.lambdaBody testCase
            (Types.var "t0")
        H.it "case #3" $
          expectTypeAnnotation (Expect.lambdaBody >=> Expect.letBinding "alias") testCase
            (Types.var "t0")
      H.describe "test #3" $ do
        let testCase = lambda "fun" $ lambda "t" $
                         ((var "funAlias" @@ var "t") `with` [
                           "funAlias">: var "fun"])
        let funType = Types.function (Types.var "t1") (Types.var "t2")
        H.it "case #1" $
          expectTypeAnnotation pure testCase
            (Types.function funType funType)
        H.it "case #2" $
          expectTypeAnnotation (Expect.lambdaBody >=> Expect.lambdaBody) testCase
            (Types.var "t2")
        H.it "case #3" $
          expectTypeAnnotation (Expect.lambdaBody >=> Expect.lambdaBody >=> Expect.letBinding "funAlias") testCase
            funType
  where
    tmp term = shouldSucceedWith flow ()
      where
        flow = do
          iterm <- inferTermType term
          fail $ "iterm: " ++ show iterm

--checkTypedTerms :: H.SpecWith ()
--checkTypedTerms = H.describe "Check that term/type pairs are consistent with type inference" $ do
--
--    H.it "Check arbitrary typed terms" $
--      QC.property $ \(TypedTerm term typ) -> expectMonotype term typ

spec :: H.Spec
spec = do
  checkLiterals
  checkSubtermAnnotations
  checkTypeAnnotations
--  checkTypedTerms
