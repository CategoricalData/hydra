module Hydra.CoreCodersSpec where

import Hydra.Kernel
import Hydra.Dsl.Terms as Terms
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Meta
import qualified Hydra.Dsl.Types as Types

import Hydra.TestData
import Hydra.TestUtils
import Hydra.ArbitraryCore (untyped)

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Map as M


individualEncoderTestCases :: H.SpecWith ()
individualEncoderTestCases = do
  H.describe "Individual encoder test cases" $ do

    H.it "string literal type" $ do
      H.shouldBe
        (strip $ encodeLiteralType LiteralTypeString :: Term Meta)
        (strip $ unitVariant _LiteralType _LiteralType_string)

    H.it "string type" $ do
      H.shouldBe
        (strip $ encodeType Types.string :: Term Meta)
        (strip $ variant _Type _Type_literal (unitVariant _LiteralType _LiteralType_string))

    H.it "int32 type" $ do
      H.shouldBe
        (strip $ encodeType Types.int32 :: Term Meta)
        (strip $ variant _Type _Type_literal (variant _LiteralType _LiteralType_integer $ unitVariant _IntegerType _IntegerType_int32))

    H.it "record type" $ do
      H.shouldBe
        (strip $ encodeType (TypeRecord $ RowType (Name "Example") Nothing
          [Types.field "something" Types.string, Types.field "nothing" Types.unit]) :: Term Meta)
        (strip $ variant _Type _Type_record $
          record _RowType [
            Field _RowType_typeName $ string "Example",
            Field _RowType_extends $ optional Nothing,
            Field _RowType_fields $ list [
              record _FieldType [
                Field _FieldType_name $ string "something",
                Field _FieldType_type $ variant _Type _Type_literal $ unitVariant _LiteralType _LiteralType_string],
              record _FieldType [
                Field _FieldType_name $ string "nothing",
                Field _FieldType_type $ variant _Type _Type_record $ record _RowType [
                  Field _RowType_typeName $ string "hydra/core.UnitType",
                  Field _RowType_extends $ optional Nothing,
                  Field _RowType_fields $ list []]]]])

individualDecoderTestCases :: H.SpecWith ()
individualDecoderTestCases = do
  H.describe "Individual decoder test cases" $ do

    H.it "float32 literal type" $ do
      shouldSucceedWith
        (decodeLiteralType
          (variant _LiteralType _LiteralType_float $ unitVariant _FloatType _FloatType_float32))
        (LiteralTypeFloat FloatTypeFloat32)

    H.it "float32 type" $ do
      shouldSucceedWith
        (decodeType
          (variant _Type _Type_literal $ variant _LiteralType _LiteralType_float $ unitVariant _FloatType _FloatType_float32))
        Types.float32

    H.it "union type" $ do
      shouldSucceedWith
        (decodeType $
          variant _Type _Type_union $ record _RowType [
            Field _RowType_typeName $ string (unName testTypeName),
            Field _RowType_extends $ optional Nothing,
            Field _RowType_fields $
              list [
                record _FieldType [
                  Field _FieldType_name $ string "left",
                  Field _FieldType_type $ variant _Type _Type_literal $ variant _LiteralType _LiteralType_integer $
                    unitVariant _IntegerType _IntegerType_int64],
                record _FieldType [
                  Field _FieldType_name $ string "right",
                  Field _FieldType_type $ variant _Type _Type_literal $ variant _LiteralType _LiteralType_float $
                    unitVariant _FloatType _FloatType_float64]]])
          (TypeUnion $ RowType testTypeName Nothing [
            Types.field "left" Types.int64,
            Types.field "right" Types.float64])

decodeInvalidTerms :: H.SpecWith ()
decodeInvalidTerms = do
  H.describe "Decode invalid terms" $ do

    H.it "Try to decode a term with wrong fields for Type" $ do
      shouldFail (decodeType $ variant untyped (FieldName "unknownField") $ list [])

    H.it "Try to decode an incomplete representation of a Type" $ do
      shouldFail (decodeType $ variant _Type _Type_literal $ unitVariant _LiteralType _LiteralType_integer)

metadataIsPreserved :: H.SpecWith ()
metadataIsPreserved = do
  H.describe "Check that metadata is preserved through a type-encoding round trip" $ do

    H.it "Basic metadata" $ do
      shouldSucceedWith
        (decodeType $ encodeType annotatedStringType)
        annotatedStringType
  where
    annotatedStringType :: Type Meta
    annotatedStringType = TypeAnnotated $ Annotated Types.string $ Meta $ M.fromList [
      (metaDescription, Terms.string "The string literal type"),
      (metaType, encodeType $ Types.wrap _Type)]

testRoundTripsFromType :: H.SpecWith ()
testRoundTripsFromType = do
  H.describe "Check that encoding, then decoding random types is a no-op" $ do

    H.it "Try random types" $
      QC.property $ \typ ->
        shouldSucceedWith
          (decodeType $ encodeType typ)
          typ

spec :: H.Spec
spec = do
  individualEncoderTestCases
  individualDecoderTestCases
  decodeInvalidTerms
  metadataIsPreserved
  testRoundTripsFromType
