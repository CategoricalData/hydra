module Hydra.Prototyping.CoreCodersSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.CoreDecoding
import Hydra.Prototyping.CoreEncoding

import Hydra.TestUtils
import Hydra.ArbitraryCore (untyped)

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


individualEncoderTestCases :: H.SpecWith ()
individualEncoderTestCases = do
  H.describe "Individual encoder test cases" $ do
    
    H.it "string literal type" $ do
      H.shouldBe
        (encodeLiteralType LiteralTypeString)
        (mterm (unitVariant _LiteralType _LiteralType_string))

    H.it "string type" $ do
      H.shouldBe
        (encodeType stringType)
        (mterm (nominal _Type $ variant _Type _Type_literal (unitVariant _LiteralType _LiteralType_string)))

    H.it "int32 type" $ do
      H.shouldBe
        (encodeType int32Type)
        (mterm (nominal _Type $
          variant _Type _Type_literal (variant _LiteralType _LiteralType_integer $ unitVariant _IntegerType _IntegerType_int32)))

    H.it "record type" $ do
      H.shouldBe
        (encodeType (TypeRecord [FieldType "something" stringType, FieldType "nothing" unitType]))
        (mterm (nominal _Type $ variant _Type _Type_record $ list [
          nominal _FieldType $ record [
            Field _FieldType_name $ stringValue "something",
            Field _FieldType_type $ nominal _Type $ variant _Type _Type_literal $ unitVariant _LiteralType _LiteralType_string],
          nominal _FieldType $ record [
            Field _FieldType_name $ stringValue "nothing",
            Field _FieldType_type $ nominal _Type $ variant _Type _Type_record $ list []]]))

individualDecoderTestCases :: H.SpecWith ()
individualDecoderTestCases = do
  H.describe "Individual decoder test cases" $ do
    
    H.it "float32 literal type" $ do
      decodeLiteralType testContext (variant _LiteralType _LiteralType_float $ unitVariant _FloatType _FloatType_float32) `H.shouldBe`
        pure (LiteralTypeFloat FloatTypeFloat32)

    H.it "float32 type" $ do
      decodeType testContext (variant _Type _Type_literal $ variant _LiteralType _LiteralType_float $ unitVariant _FloatType _FloatType_float32)
        `H.shouldBe` pure float32Type
        
    H.it "union type" $ do
      decodeType testContext (variant _Type _Type_union $ list [
        nominal _FieldType $ record [
          Field _FieldType_name $ stringValue "left",
          Field _FieldType_type $ variant _Type _Type_literal $ variant _LiteralType _LiteralType_integer $ unitVariant _IntegerType _IntegerType_int64],
        nominal _FieldType $ record [
          Field _FieldType_name $ stringValue "right",
          Field _FieldType_type $ variant _Type _Type_literal $ variant _LiteralType _LiteralType_float $ unitVariant _FloatType _FloatType_float64]])
        `H.shouldBe` pure (TypeUnion [FieldType "left" int64Type, FieldType "right" float64Type])

decodeInvalidTerms :: H.SpecWith ()
decodeInvalidTerms = do
  H.describe "Decode invalid terms" $ do
      
    H.it "Try to decode a term with wrong fields for Type" $ do
      isFailure (decodeType testContext $ variant untyped "unknownField" $ list []) `H.shouldBe` True

    H.it "Try to decode an incomplete representation of a Type" $ do
      isFailure (decodeType testContext$ variant _Type _Type_literal $ unitVariant _LiteralType _LiteralType_integer) `H.shouldBe` True

mterm :: Term Meta -> Term Meta
mterm = id

testRoundTripsFromType :: H.SpecWith ()
testRoundTripsFromType = do
  H.describe "Check that encoding, then decoding random types is a no-op" $ do
    
    H.it "Try random types" $
      QC.property $ \typ -> decodeType testContext (encodeType typ) == pure typ

spec :: H.Spec
spec = do
  individualEncoderTestCases
  individualDecoderTestCases
  decodeInvalidTerms
  testRoundTripsFromType
