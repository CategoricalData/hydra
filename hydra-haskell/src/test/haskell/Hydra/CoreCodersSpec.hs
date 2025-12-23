{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.CoreCodersSpec.spec
-}

module Hydra.CoreCodersSpec where

import Hydra.Kernel
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Monads as Monads
import qualified Hydra.Util as Util

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
        (strip $ EncodeCore.literalType LiteralTypeString :: Term)
        (strip $ injectUnit _LiteralType _LiteralType_string)

    H.it "string type" $ do
      H.shouldBe
        (strip $ EncodeCore.type_ Types.string :: Term)
        (strip $ inject _Type _Type_literal (injectUnit _LiteralType _LiteralType_string))

    H.it "int32 type" $ do
      H.shouldBe
        (strip $ EncodeCore.type_ Types.int32 :: Term)
        (strip $ inject _Type _Type_literal (inject _LiteralType _LiteralType_integer $ injectUnit _IntegerType _IntegerType_int32))

    H.it "record type" $ do
      H.shouldBe
        (strip $ EncodeCore.type_ (TypeRecord $ RowType (Name "Example")
          [Types.field "something" Types.string, Types.field "nothing" Types.unit]) :: Term)
        (strip $ inject _Type _Type_record $
          record _RowType [
            Field _RowType_typeName $ wrap _Name $ string "Example",
            Field _RowType_fields $ list [
              record _FieldType [
                Field _FieldType_name $ wrap _Name $ string "something",
                Field _FieldType_type $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_string],
              record _FieldType [
                Field _FieldType_name $ wrap _Name $ string "nothing",
                Field _FieldType_type $ injectUnit _Type _Type_unit]]])

    H.it "Name (wrapped type)" $ do
      H.shouldBe
        (strip $ EncodeCore.name (Name "test.Name") :: Term)
        (strip $ wrap _Name $ string "test.Name")

    H.it "string literal" $ do
      H.shouldBe
        (strip $ EncodeCore.literal (LiteralString "hello") :: Term)
        (strip $ inject _Literal _Literal_string $ string "hello")

    H.it "int32 literal" $ do
      H.shouldBe
        (strip $ EncodeCore.literal (LiteralInteger (IntegerValueInt32 42)) :: Term)
        (strip $ inject _Literal _Literal_integer $ inject _IntegerValue _IntegerValue_int32 $ int32 42)

    H.it "boolean literal" $ do
      H.shouldBe
        (strip $ EncodeCore.literal (LiteralBoolean True) :: Term)
        (strip $ inject _Literal _Literal_boolean $ boolean True)

    H.it "Field (record type)" $ do
      H.shouldBe
        (strip $ EncodeCore.field (Field (Name "myField") (Terms.string "value")) :: Term)
        (strip $ record _Field [
          Field _Field_name $ wrap _Name $ string "myField",
          Field _Field_term $ inject _Term _Term_literal $ inject _Literal _Literal_string $ string "value"])

    H.it "Lambda function" $ do
      H.shouldBe
        (strip $ EncodeCore.lambda (Lambda (Name "x") Nothing (TermVariable (Name "x"))) :: Term)
        (strip $ record _Lambda [
          Field _Lambda_parameter $ wrap _Name $ string "x",
          Field _Lambda_domain $ optional Nothing,
          Field _Lambda_body $ inject _Term _Term_variable $ wrap _Name $ string "x"])

    H.it "function type" $ do
      H.shouldBe
        (strip $ EncodeCore.type_ (Types.function Types.string Types.int32) :: Term)
        (strip $ inject _Type _Type_function $ record _FunctionType [
          Field _FunctionType_domain $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_string,
          Field _FunctionType_codomain $ inject _Type _Type_literal $ inject _LiteralType _LiteralType_integer $
            injectUnit _IntegerType _IntegerType_int32])

    H.it "list type" $ do
      H.shouldBe
        (strip $ EncodeCore.type_ (Types.list Types.boolean) :: Term)
        (strip $ inject _Type _Type_list $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_boolean)

    H.it "optional type" $ do
      H.shouldBe
        (strip $ EncodeCore.type_ (Types.optional Types.string) :: Term)
        (strip $ inject _Type _Type_maybe $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_string)

individualDecoderTestCases :: H.SpecWith ()
individualDecoderTestCases = do
  H.describe "Individual decoder test cases" $ do

    H.it "float32 literal type" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.literalType testGraph
          (inject _LiteralType _LiteralType_float $ injectUnit _FloatType _FloatType_float32))
        (LiteralTypeFloat FloatTypeFloat32)

    H.it "float32 type" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph
          (inject _Type _Type_literal $ inject _LiteralType _LiteralType_float $ injectUnit _FloatType _FloatType_float32))
        Types.float32

    H.it "union type" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $
          inject _Type _Type_union $ record _RowType [
            Field _RowType_typeName $ wrap _Name $ string (unName testTypeName),
            Field _RowType_fields $
              list [
                record _FieldType [
                  Field _FieldType_name $ wrap _Name $ string "left",
                  Field _FieldType_type $ inject _Type _Type_literal $ inject _LiteralType _LiteralType_integer $
                    injectUnit _IntegerType _IntegerType_int64],
                record _FieldType [
                  Field _FieldType_name $ wrap _Name $ string "right",
                  Field _FieldType_type $ inject _Type _Type_literal $ inject _LiteralType _LiteralType_float $
                    injectUnit _FloatType _FloatType_float64]]])
          (TypeUnion $ RowType testTypeName [
            Types.field "left" Types.int64,
            Types.field "right" Types.float64])

    H.it "Name (wrapped type)" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.name testGraph $
          wrap _Name $ string "test.Name")
        (Name "test.Name")

    H.it "string literal" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.literal testGraph $
          inject _Literal _Literal_string $ string "hello")
        (LiteralString "hello")

    H.it "int32 literal" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.literal testGraph $
          inject _Literal _Literal_integer $ inject _IntegerValue _IntegerValue_int32 $ int32 42)
        (LiteralInteger (IntegerValueInt32 42))

    H.it "boolean literal" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.literal testGraph $
          inject _Literal _Literal_boolean $ boolean True)
        (LiteralBoolean True)

    H.it "Field (record type)" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.field testGraph $
          record _Field [
            Field _Field_name $ wrap _Name $ string "myField",
            Field _Field_term $ inject _Term _Term_literal $ inject _Literal _Literal_string $ string "value"])
        (Field (Name "myField") (Terms.string "value"))

    H.it "Lambda function" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.lambda testGraph $
          record _Lambda [
            Field _Lambda_parameter $ wrap _Name $ string "x",
            Field _Lambda_domain $ optional Nothing,
            Field _Lambda_body $ inject _Term _Term_variable $ wrap _Name $ string "x"])
        (Lambda (Name "x") Nothing (TermVariable (Name "x")))

    H.it "function type" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $
          inject _Type _Type_function $ record _FunctionType [
            Field _FunctionType_domain $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_string,
            Field _FunctionType_codomain $ inject _Type _Type_literal $ inject _LiteralType _LiteralType_integer $
              injectUnit _IntegerType _IntegerType_int32])
        (Types.function Types.string Types.int32)

    H.it "list type" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $
          inject _Type _Type_list $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_boolean)
        (Types.list Types.boolean)

    H.it "optional type" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $
          inject _Type _Type_maybe $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_string)
        (Types.optional Types.string)

decodeInvalidTerms :: H.SpecWith ()
decodeInvalidTerms = do
  H.describe "Decode invalid terms" $ do

    H.it "Try to decode a term with wrong fields for Type" $ do
      shouldFail (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $ inject untyped (Name "unknownField") $ list [])

    H.it "Try to decode an incomplete representation of a Type" $ do
      shouldFail (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $ inject _Type _Type_literal $ injectUnit _LiteralType _LiteralType_integer)

metadataIsPreserved :: H.SpecWith ()
metadataIsPreserved = do
  H.describe "Check that metadata is preserved through a type-encoding round trip" $ do

    H.it "Basic metadata" $ do
      shouldSucceedWith
        (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $ EncodeCore.type_ annotatedStringType)
        annotatedStringType
  where
    annotatedStringType :: Type
    annotatedStringType = TypeAnnotated $ AnnotatedType Types.string $ M.fromList [
      (key_description, Terms.string "The string literal type"),
      (key_type, EncodeCore.type_ $ TypeVariable _Type)]

testRoundTripsFromType :: H.SpecWith ()
testRoundTripsFromType = do
  H.describe "Check that encoding, then decoding random types is a no-op" $ do

    H.it "Try random types" $
      QC.property $ \typ ->
        shouldSucceedWith
          (Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ testGraph $ EncodeCore.type_ typ)
          typ

spec :: H.Spec
spec = do
  individualEncoderTestCases
  individualDecoderTestCases
  decodeInvalidTerms
  metadataIsPreserved
  testRoundTripsFromType
