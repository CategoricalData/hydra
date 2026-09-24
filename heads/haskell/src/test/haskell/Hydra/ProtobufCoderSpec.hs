{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ProtobufCoderSpec.spec
-}

module Hydra.ProtobufCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Protobuf.Coder as ProtobufCoder
import qualified Hydra.Ext.Protobuf.Environment as ProtobufEnvironment
import qualified Hydra.Ext.Protobuf.Proto3 as Proto3

import Hydra.TestUtils

import qualified Test.Hspec as H


-- | Byte-parity regression test for #654: the Protobuf coder's fallback wrap-type handling
-- (any non-record, non-enum-union type) used to be built by a coder-local let-bound helper
-- (wrapAsRecordType) that inlined a single-field "value" record. That helper was deleted in
-- favor of the shared kernel function Rewriting.wrapTypeToRecord; this test confirms the
-- generated Protobuf message definition is unchanged by the refactor -- same single "value"
-- field, same wrapped type.
spec :: H.Spec
spec = H.describe "Protobuf.Coder.encodeDefinition on wrap-shaped types (#654 byte-parity)" $ do

  H.it "wraps a bare string type in a single-field 'value' message" $ do
    assertSingleValueField (TypeLiteral LiteralTypeString)

  H.it "wraps a TypeWrap(string) type in a single-field 'value' message" $ do
    assertSingleValueField (TypeWrap (TypeLiteral LiteralTypeString))

  -- #744: Protobuf has no int8/int16/uint8/uint16/bigint scalar, so encodeScalarType used to
  -- hard-error on these widths despite them being ordinary, supported Hydra integer types. It now
  -- widens int8/int16 (losslessly) to int32, uint8/uint16 (losslessly) to uint32, and narrows
  -- bigint (lossily, matching the Avro coder's own lossy fallback) to int64.
  H.describe "Protobuf.Coder.encodeScalarType on integer widths without a native scalar (#744)" $ do

    H.it "int8 widens to int32" $ do
      mapError (ProtobufCoder.encodeScalarType testEncoderState (LiteralTypeInteger IntegerTypeInt8))
        `H.shouldBe` Right Proto3.ScalarTypeInt32

    H.it "int16 widens to int32" $ do
      mapError (ProtobufCoder.encodeScalarType testEncoderState (LiteralTypeInteger IntegerTypeInt16))
        `H.shouldBe` Right Proto3.ScalarTypeInt32

    H.it "uint8 widens to uint32" $ do
      mapError (ProtobufCoder.encodeScalarType testEncoderState (LiteralTypeInteger IntegerTypeUint8))
        `H.shouldBe` Right Proto3.ScalarTypeUint32

    H.it "uint16 widens to uint32" $ do
      mapError (ProtobufCoder.encodeScalarType testEncoderState (LiteralTypeInteger IntegerTypeUint16))
        `H.shouldBe` Right Proto3.ScalarTypeUint32

    H.it "bigint narrows (lossily) to int64" $ do
      mapError (ProtobufCoder.encodeScalarType testEncoderState (LiteralTypeInteger IntegerTypeBigint))
        `H.shouldBe` Right Proto3.ScalarTypeInt64

  where
    testEncoderState = ProtobufEnvironment.EncoderState testContext 0
    testName = Name "test.Wrapped"
    testNs = ModuleName "test"

    assertSingleValueField :: Type -> H.Expectation
    assertSingleValueField innerType =
      case mapError $ ProtobufCoder.encodeDefinition testEncoderState testGraph testNs testName innerType of
        Left e -> H.expectationFailure $ "encodeDefinition failed: " ++ e
        Right def -> fieldNames def `H.shouldBe` ["value"]

    fieldNames :: Proto3.Definition -> [String]
    fieldNames (Proto3.DefinitionMessage md) =
      map (Proto3.unFieldName . Proto3.fieldName) (Proto3.messageDefinitionFields md)
    fieldNames _ = []
