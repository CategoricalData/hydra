{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.JsonSchemaCoderSpec.spec
-}

module Hydra.JsonSchemaCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Json.Schema.Coder as JsonSchemaCoder
import qualified Hydra.Ext.Json.Schema.Model as JS

import qualified Test.Hspec as H


-- | #744: literalTypeName used to map every IntegerType width to JSON Schema "integer",
-- disagreeing with the JSON value coder (Hydra.Core.Json.Encode.encodeInteger), which writes int64,
-- uint64, and bigint as JSON strings (to preserve precision beyond 2^53-1) rather than numbers.
-- A schema generated from literalTypeName would then reject the value coder's own output for
-- those three widths. These tests confirm literalTypeName is now width-aware: only widths that
-- the value coder actually writes as a JSON number map to "integer"; the three string-encoded
-- widths map to "string".
spec :: H.Spec
spec = H.describe "Json.Schema.Coder.literalTypeName integer-width agreement with the value coder (#744)" $ do

  H.it "int8 maps to \"integer\" (value coder writes it as a JSON number)" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeInt8) `H.shouldBe` JS.TypeNameInteger

  H.it "int16 maps to \"integer\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeInt16) `H.shouldBe` JS.TypeNameInteger

  H.it "int32 maps to \"integer\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeInt32) `H.shouldBe` JS.TypeNameInteger

  H.it "uint8 maps to \"integer\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeUint8) `H.shouldBe` JS.TypeNameInteger

  H.it "uint16 maps to \"integer\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeUint16) `H.shouldBe` JS.TypeNameInteger

  H.it "uint32 maps to \"integer\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeUint32) `H.shouldBe` JS.TypeNameInteger

  H.it "int64 maps to \"string\" (value coder writes it as a JSON string, not a number)" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeInt64) `H.shouldBe` JS.TypeNameString

  H.it "uint64 maps to \"string\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeUint64) `H.shouldBe` JS.TypeNameString

  H.it "bigint maps to \"string\"" $ do
    JsonSchemaCoder.literalTypeName (LiteralTypeInteger IntegerTypeBigint) `H.shouldBe` JS.TypeNameString

  H.it "binary maps to \"string\" (unchanged -- already agreed with the value coder's base64 string)" $ do
    JsonSchemaCoder.literalTypeName LiteralTypeBinary `H.shouldBe` JS.TypeNameString
