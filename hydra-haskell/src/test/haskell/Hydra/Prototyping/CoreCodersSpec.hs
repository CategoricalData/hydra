module Hydra.Prototyping.CoreCodersSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Prototyping.CoreDecoding
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Extras

import Hydra.TestUtils
import Hydra.ArbitraryCore (untyped)

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


individualEncoderTestCases :: H.SpecWith ()
individualEncoderTestCases = do
  H.describe "Individual encoder test cases" $ do

    H.it "string literal type" $ do
      H.shouldBe
        (stripMeta $ encodeLiteralType testContext LiteralTypeString)
        (stripMeta $  unitVar _LiteralType _LiteralType_string)

    H.it "string type" $ do
      H.shouldBe
        (stripMeta $ encodeType testContext stringType)
        (stripMeta $  var _Type _Type_literal (unitVar _LiteralType _LiteralType_string))

    H.it "int32 type" $ do
      H.shouldBe
        (stripMeta $ encodeType testContext int32Type)
        (stripMeta $ var _Type _Type_literal (var _LiteralType _LiteralType_integer $ unitVar _IntegerType _IntegerType_int32))

    H.it "record type" $ do
      H.shouldBe
        (stripMeta $ encodeType testContext (recordType [FieldType "something" stringType, FieldType "nothing" unitType]))
        (stripMeta $ var _Type _Type_record $ list [
          record [
            Field _FieldType_name $ stringValue "something",
            Field _FieldType_type $ var _Type _Type_literal $ unitVar _LiteralType _LiteralType_string],
          record [
            Field _FieldType_name $ stringValue "nothing",
            Field _FieldType_type $ var _Type _Type_record $ list []]])

individualDecoderTestCases :: H.SpecWith ()
individualDecoderTestCases = do
  H.describe "Individual decoder test cases" $ do

    H.it "float32 literal type" $ do
      decodeLiteralType testContext (var _LiteralType _LiteralType_float $ unitVar _FloatType _FloatType_float32) `H.shouldBe`
        pure (LiteralTypeFloat FloatTypeFloat32)

    H.it "float32 type" $ do
      decodeType testContext (var _Type _Type_literal $ var _LiteralType _LiteralType_float $ unitVar _FloatType _FloatType_float32)
        `H.shouldBe` pure float32Type

    H.it "union type" $ do
      decodeType testContext (var _Type _Type_union $ list [
        record [
          Field _FieldType_name $ stringValue "left",
          Field _FieldType_type $ var _Type _Type_literal $ var _LiteralType _LiteralType_integer $ unitVar _IntegerType _IntegerType_int64],
        record [
          Field _FieldType_name $ stringValue "right",
          Field _FieldType_type $ var _Type _Type_literal $ var _LiteralType _LiteralType_float $ unitVar _FloatType _FloatType_float64]])
        `H.shouldBe` pure (TypeUnion [FieldType "left" int64Type, FieldType "right" float64Type])

decodeInvalidTerms :: H.SpecWith ()
decodeInvalidTerms = do
  H.describe "Decode invalid terms" $ do

    H.it "Try to decode a term with wrong fields for Type" $ do
      isFailure (decodeType testContext $ var untyped "unknownField" $ list []) `H.shouldBe` True

    H.it "Try to decode an incomplete representation of a Type" $ do
      isFailure (decodeType testContext$ var _Type _Type_literal $ unitVar _LiteralType _LiteralType_integer) `H.shouldBe` True

testRoundTripsFromType :: H.SpecWith ()
testRoundTripsFromType = do
  H.describe "Check that encoding, then decoding random types is a no-op" $ do

    H.it "Try random types" $
      QC.property $ \typ -> decodeType testContext (encodeType testContext typ) == pure typ

spec :: H.Spec
spec = do
  individualEncoderTestCases
  individualDecoderTestCases
  decodeInvalidTerms
  testRoundTripsFromType

unitVar :: Name -> FieldName -> Term Meta
unitVar = nominalUnitVariant testContext

var :: Name -> FieldName -> Term Meta -> Term Meta
var = nominalVariant testContext