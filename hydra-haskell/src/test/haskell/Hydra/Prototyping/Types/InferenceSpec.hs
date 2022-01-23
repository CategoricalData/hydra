module Hydra.Prototyping.Types.InferenceSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Basics
import Hydra.Prototyping.Types.Inference
import Hydra.TestUtils
import Hydra.Impl.Haskell.Sources.Adapters.Utils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import qualified Data.Set as S


checkType :: Term (Meta, Type, [Constraint]) -> Type -> H.Expectation
checkType term typ = typeAnn term `H.shouldBe` typ
  where
    typeAnn (Term _ (_, typ, _)) = typ

expectMonotype :: Term Meta -> Type -> H.Expectation
expectMonotype term = expectPolytype term []

expectPolytype :: Term Meta-> [TypeVariable] -> Type -> H.Expectation
expectPolytype term vars typ = do
    let result = inferType testContext term
    snd <$> result `H.shouldBe` ResultSuccess (TypeScheme vars typ)
--    typeAnn . fst <$> result `H.shouldBe` ResultSuccess typ -- TODO: re-enable me
  where
    typeAnn (Term _ (_, typ, _)) = typ

checkApplicationTerms :: H.SpecWith ()
checkApplicationTerms = do
  H.describe "Check a few hand-picked application terms" $ do

    H.it "Check lambda applications" $ do
      expectMonotype
        (apply (lambda "x" (variable "x")) (stringValue "foo"))
        stringType

    H.it "Check data (delta) applications" $ do
      expectMonotype
        (apply dataTerm (element "ArthurDent"))
        testTypePerson
--      expectMonotype
--        (apply dataTerm describeType)
--        (functionType (nominalType _Type) stringType)

    H.it "Check mixed expressions with lambdas, constants, and primitive functions" $ do
      expectMonotype
        (lambda "x" $
          apply
            (apply (primitive _math_sub) (apply (apply (primitive _math_add) (variable "x")) (variable "x")))
            (int32Value 1))
        (functionType int32Type int32Type)

checkFunctionTerms :: H.SpecWith ()
checkFunctionTerms = do
  H.describe "Check a few hand-picked function terms" $ do

    H.it "Check lambdas" $ do
      expectPolytype
        (lambda "x" (variable "x"))
        ["v1"] (functionType (typeVariable "v1") (typeVariable "v1"))
      expectPolytype
        (lambda "x" (int16Value 137))
        ["v1"] (functionType (typeVariable "v1") int16Type)

    H.it "Check primitive functions" $ do
      expectMonotype
        (primitive "hydra/lib/strings.length")
        (functionType stringType int32Type)
      expectMonotype
        (primitive _math_sub)
        (functionType int32Type (functionType int32Type int32Type))

    H.it "Check 'compareTo' terms" $ do
      expectMonotype
        (compareTo $ record [Field "fst" $ booleanValue True, Field "snd" $ stringValue "Betelgeuse"])
        (functionType (recordType [FieldType "fst" booleanType, FieldType "snd" stringType]) int8Type)
      expectPolytype
        (lambda "x" $ compareTo (variable "x"))
        ["v1"] (functionType (typeVariable "v1") (functionType (typeVariable "v1") int8Type))

    H.it "Check projections" $ do
      expectMonotype
        (nominalProjection testContext "Person" "firstName" stringType)
        (functionType (nominalType "Person") stringType)

    H.it "Check case statements" $ do
      expectPolytype
        (cases [
          Field "left" (lambda "x" (booleanValue True)),
          Field "right" (lambda "x" (booleanValue False))])
        ["v1", "v2"] (functionType
          (unionType [
            FieldType "left" (typeVariable "v1"),
            FieldType "right" (typeVariable "v2")])
          booleanType)
      expectPolytype
        (cases [
          Field "person" (apply dataTerm (element "firstName")),
          Field "other" (lambda "x" (stringValue "NONE"))])
        ["v1"] (functionType
          (unionType [
            FieldType "person" (nominalType "Person"),
            FieldType "other" (typeVariable "v1")])
          stringType)

checkIndividualTerms :: H.SpecWith ()
checkIndividualTerms = do
  H.describe "Check a few hand-picked terms" $ do

    H.it "Check literal values" $ do
      expectMonotype
        (int32Value 42)
        int32Type
      expectMonotype
        (stringValue "foo")
        stringType
      expectMonotype
        (booleanValue False)
        booleanType
      expectMonotype
        (float64Value 42.0)
        float64Type

    H.it "Check let terms" $ do
      expectPolytype
        (letTerm "x" (float32Value 42.0) (lambda "y" (lambda "z" (variable "x"))))
        ["v1", "v2"] (functionType (typeVariable "v1") (functionType (typeVariable "v2") float32Type))

    H.it "Check elements" $ do
      expectMonotype
        (element "ArthurDent")
        (elementType testTypePerson) -- Note: the resolved element type is the raw record type associated with "Person", not the nominal type "Person".
      expectMonotype
        (element "firstName")
        (elementType (functionType (nominalType "Person") stringType))

    H.it "Check optionals" $ do
      expectMonotype
        (optional $ Just $ int32Value 42)
        (optionalType int32Type)
      expectPolytype
        (optional Nothing)
        ["v1"] (optionalType $ typeVariable "v1")

    H.it "Check records" $ do
      expectMonotype
        (record [Field "lat" $ float64Value 37.7749, Field "lon" $ float64Value $ negate 122.4194])
        (recordType [FieldType "lat" float64Type, FieldType "lon" float64Type])
      expectPolytype
        (lambda "lon" (record [Field "lat" $ float64Value 37.7749, Field "lon" $ variable "lon"]))
        ["v1"] (functionType (typeVariable "v1") (recordType [FieldType "lat" float64Type, FieldType "lon" $ typeVariable "v1"]))

    H.it "Check unions" $ do
      expectMonotype
        (nominalUnion testContext "Timestamp" $ Field "unixTimeMillis" $ uint64Value 1638200308368)
        (nominalType "Timestamp")

    H.it "Check lists" $ do
      expectMonotype
        (list [stringValue "foo", stringValue "bar"])
        (listType stringType)
      expectPolytype
        (list [])
        ["v1"] (listType $ typeVariable "v1")
      expectMonotype
        (lambda "x" (list [variable "x", int32Value 42]))
        (functionType int32Type $ listType int32Type)

    H.it "Check sets" $ do
      expectMonotype
        (set $ S.fromList [booleanValue True])
        (setType booleanType)

    H.it "Check maps" $ do
      expectMonotype
        (mapTerm $ M.fromList [(stringValue "firstName", stringValue "Arthur"), (stringValue "lastName", stringValue "Dent")])
        (mapType stringType stringType)
      expectPolytype
        (mapTerm M.empty)
        ["v1", "v2"] (mapType (typeVariable "v1") (typeVariable "v2"))
      expectPolytype
        (lambda "x" (lambda "y" (mapTerm $ M.fromList
          [(variable "x", float64Value 0.1), (variable "y", float64Value 0.2)])))
        ["v1"] (functionType (typeVariable "v1") (functionType (typeVariable "v1") (mapType (typeVariable "v1") float64Type)))

    -- TODO: restore me, and add a case for a recursive nominal type -- e.g. MyList := () + (int, Mylist)
--    H.it "Check nominal (newtype) terms" $ do
--      expectMonotype
--        testTermArthur
--        (nominalType "Person")
--      expectMonotype
--        (lambda "x" (record [
--          Field "firstName" $ variable "x",
--          Field "lastName" $ variable "x",
--          Field "age" $ int32Value 42]))
--        (functionType stringType testTypePerson)

checkLiterals :: H.SpecWith ()
checkLiterals = do
  H.describe "Check arbitrary literals" $ do

    H.it "Verify that type inference preserves the literal to literal type mapping" $
      QC.property $ \l -> expectMonotype
        (defaultTerm $ ExpressionLiteral l)
        (TypeLiteral $ literalType l)

checkTypeAnnotations :: H.SpecWith ()
checkTypeAnnotations = do
  H.describe "Check that type annotations are added to terms and subterms" $ do

    H.it "Check literals" $
      QC.property $ \l -> do
        let term = defaultTerm $ ExpressionLiteral l
        let (ResultSuccess term1) = fst <$> inferType testContext term
        checkType term1 (TypeLiteral $ literalType l)

    H.it "Check lists of literals" $
      QC.property $ \l -> do
        let term = defaultTerm $ ExpressionList [defaultTerm $ ExpressionLiteral l]
        let (ResultSuccess term1) = fst <$> inferType testContext term
        checkType term1 (TypeList $ TypeLiteral $ literalType l)
        let (ExpressionList [term2]) = termData term1
        checkType term2 (TypeLiteral $ literalType l)

checkTypedTerms :: H.SpecWith ()
checkTypedTerms = do
  H.describe "Check that term/type pairs are consistent with type inference" $ do

    H.it "Check arbitrary typed terms" $
      QC.property $ \(TypedTerm typ term) -> expectMonotype term typ

spec :: H.Spec
spec = do
  checkApplicationTerms
  checkFunctionTerms
  checkIndividualTerms
  checkLiterals
  checkTypeAnnotations
--  checkTypedTerms
