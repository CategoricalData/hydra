module Hydra.CoreCodersSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Meta
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

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
        (strip $ unitVariant _LiteralType_string)

    H.it "string type" $ do
      H.shouldBe
        (strip $ encodeType testContext Types.string)
        (strip $  variant _Type_literal (unitVariant _LiteralType_string))

    H.it "int32 type" $ do
      H.shouldBe
        (strip $ encodeType testContext Types.int32)
        (strip $ variant _Type_literal (variant _LiteralType_integer $ unitVariant _IntegerType_int32))

    H.it "record type" $ do
      H.shouldBe
        (strip $ encodeType testContext (Types.record [Types.field "something" Types.string, Types.field "nothing" Types.unit]))
        (strip $ variant _Type_record $ list [
          record [
            Field _FieldType_name $ string "something",
            Field _FieldType_type $ variant _Type_literal $ unitVariant _LiteralType_string],
          record [
            Field _FieldType_name $ string "nothing",
            Field _FieldType_type $ variant _Type_record $ list []]])

individualDecoderTestCases :: H.SpecWith ()
individualDecoderTestCases = do
  H.describe "Individual decoder test cases" $ do

    H.it "float32 literal type" $ do
      decodeLiteralType testContext (variant _LiteralType_float $ unitVariant _FloatType_float32) `H.shouldBe`
        pure (LiteralTypeFloat FloatTypeFloat32)

    H.it "float32 type" $ do
      decodeType testContext (variant _Type_literal $ variant _LiteralType_float $ unitVariant _FloatType_float32)
        `H.shouldBe` pure Types.float32

    H.it "union type" $ do
      decodeType testContext (variant _Type_union $ list [
        record [
          Field _FieldType_name $ string "left",
          Field _FieldType_type $ variant _Type_literal $ variant _LiteralType_integer $ unitVariant _IntegerType_int64],
        record [
          Field _FieldType_name $ string "right",
          Field _FieldType_type $ variant _Type_literal $ variant _LiteralType_float $ unitVariant _FloatType_float64]])
        `H.shouldBe` pure (Types.union [Types.field "left" Types.int64, Types.field "right" Types.float64])

decodeInvalidTerms :: H.SpecWith ()
decodeInvalidTerms = do
  H.describe "Decode invalid terms" $ do

    H.it "Try to decode a term with wrong fields for Type" $ do
      isFailure (decodeType testContext $ nominalVariant testContext untyped (FieldName "unknownField") $ list []) `H.shouldBe` True

    H.it "Try to decode an incomplete representation of a Type" $ do
      isFailure (decodeType testContext$ variant _Type_literal $ unitVariant _LiteralType_integer) `H.shouldBe` True

metadataIsPreserved :: H.SpecWith ()
metadataIsPreserved = do
  H.describe "Check that metadata is preserved through a type-encoding round trip" $ do

    H.it "Basic metadata" $ do
      H.shouldBe
        (decodeType testContext $ encodeType testContext annotatedStringType)
        (pure annotatedStringType)
  where
    annotatedStringType :: Type Meta
    annotatedStringType = TypeAnnotated $ Annotated Types.string $ Meta $ M.fromList [
      (metaDescription, Terms.string "The string literal type"),
      (metaType, encodeType testContext $ Types.nominal _Type)]

testRoundTripsFromType :: H.SpecWith ()
testRoundTripsFromType = do
  H.describe "Check that encoding, then decoding random types is a no-op" $ do

    H.it "Try random types" $
      QC.property $ \typ -> 
        H.shouldBe
          (decodeType testContext (encodeType testContext typ))
          (pure typ)

spec :: H.Spec
spec = do
  individualEncoderTestCases
  individualDecoderTestCases
  decodeInvalidTerms
  metadataIsPreserved
  testRoundTripsFromType
