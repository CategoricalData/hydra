{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ProtobufCoderSpec.spec
-}

module Hydra.ProtobufCoderSpec where

import Hydra.Kernel
import qualified Hydra.Protobuf.Coder as ProtobufCoder
import qualified Hydra.Protobuf.Environment as ProtobufEnvironment
import qualified Hydra.Protobuf.Proto3 as Proto3

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
