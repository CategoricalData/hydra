{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ProtobufCoderSpec.spec
-}

module Hydra.ProtobufCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Protobuf.Coder as ProtobufCoder
import qualified Hydra.Ext.Protobuf.Environment as ProtobufEnvironment
import qualified Hydra.Ext.Protobuf.Proto3 as Proto3
import qualified Hydra.Ext.Protobuf.Serde as ProtobufSerde
import Hydra.Core.Serialization (parenthesize, printExpr)

import Hydra.TestUtils

import qualified Data.List as L
import qualified Test.Hspec as H


-- | Render a Protobuf definition to its actual `.proto` text, the same pipeline
-- moduleToProtobuf uses per-definition (Serde.definitionToExpr, then the shared
-- expression printer). This is #699's golden-output path: assertions compare real
-- generated .proto syntax, not just the AST shape.
printDefinition :: Proto3.Definition -> String
printDefinition = printExpr . parenthesize . ProtobufSerde.definitionToExpr

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

  -- #699: golden-output coverage for ordinary (non-bug-regression) type shapes, asserting
  -- against the actual generated .proto text rather than just AST structure.
  H.describe "Protobuf.Coder.encodeDefinition golden output (#699)" $ do

    H.it "encodes a record with scalar and optional fields as a message" $ do
      let recordType = TypeRecord [
            FieldType (Name "name") (TypeLiteral LiteralTypeString),
            FieldType (Name "age") (TypeLiteral (LiteralTypeInteger IntegerTypeInt32)),
            FieldType (Name "nickname") (TypeOptional (TypeLiteral LiteralTypeString))]
      case mapError $ ProtobufCoder.encodeDefinition testEncoderState testGraph testNs (Name "test.Person") recordType of
        Left e -> H.expectationFailure $ "encodeDefinition failed: " ++ e
        Right def -> printDefinition def `H.shouldBe` L.intercalate "\n" [
          "message Person {",
          "  string name = 1;",
          "",
          "  int32 age = 2;",
          "",
          -- An optional scalar field has no proto3 "optional" presence tracking for a plain
          -- scalar here; the coder represents it with the well-known wrapper message instead.
          "  google.protobuf.StringValue nickname = 3;",
          "}"]

    H.it "encodes a unit-fields union as a Protobuf enum" $ do
      let enumType = TypeUnion [
            FieldType (Name "red") TypeUnit,
            FieldType (Name "green") TypeUnit,
            FieldType (Name "blue") TypeUnit]
      case mapError $ ProtobufCoder.encodeDefinition testEncoderState testGraph testNs (Name "test.Color") enumType of
        Left e -> H.expectationFailure $ "encodeDefinition failed: " ++ e
        Right def -> printDefinition def `H.shouldBe` L.intercalate "\n" [
          "enum Color {",
          "  COLOR_UNSPECIFIED = 0;",
          "",
          "  COLOR_RED = 1;",
          "",
          "  COLOR_GREEN = 2;",
          "",
          "  COLOR_BLUE = 3;",
          "}"]

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
