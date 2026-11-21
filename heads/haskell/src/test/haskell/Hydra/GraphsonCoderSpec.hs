{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.GraphsonCoderSpec.spec
-}

module Hydra.GraphsonCoderSpec where

import Hydra.Kernel
import qualified Hydra.Core.Json.Model as Model
import qualified Hydra.Pg.Graphson.Coder as GraphsonCoder
import qualified Hydra.Pg.Graphson.Utils as GraphsonUtils
import qualified Hydra.Pg.Graphson.Syntax as Syntax

import Hydra.TestUtils

import qualified Test.Hspec as H


spec :: H.Spec
spec = do
  encodeLiteralValueSpec
  typeNameSpec

-- | #744: encodeLiteralValue used to handle only bigint/int32/int64 (of 8 integer widths) and
-- had no case for decimal, despite the GraphSON Value model having a dedicated slot for each
-- (byte/short/char/bigInteger/bigDecimal). Every width and decimal now encodes without error,
-- landing in the modeled slot for its width. This only checks that encoding succeeds and lands
-- on the correct Value constructor tag -- it deliberately does not assert exact native payload
-- values, since int8/uint16 widen through an intermediate width whose exact native Haskell type
-- is an implementation detail of the generic Hydra-to-Haskell integer mapping, not of this fix.
encodeLiteralValueSpec :: H.Spec
encodeLiteralValueSpec = H.describe "Pg.Graphson.Utils.encodeLiteralValue integer-width and decimal coverage (#744)" $ do

  H.it "bigint encodes to ValueBigInteger (already supported, unchanged)" $ do
    assertValueTag isBigInteger (LiteralInteger (IntegerValueBigint 42))

  H.it "int8 encodes to ValueShort (no dedicated GraphSON byte-width signed slot)" $ do
    assertValueTag isShort (LiteralInteger (IntegerValueInt8 42))

  H.it "int16 encodes to ValueShort" $ do
    assertValueTag isShort (LiteralInteger (IntegerValueInt16 42))

  H.it "int32 encodes to ValueInteger (already supported, unchanged)" $ do
    assertValueTag isInteger (LiteralInteger (IntegerValueInt32 42))

  H.it "int64 encodes to ValueLong (already supported, unchanged)" $ do
    assertValueTag isLong (LiteralInteger (IntegerValueInt64 42))

  H.it "uint8 encodes to ValueByte (the modeled uint8 slot)" $ do
    assertValueTag isByte (LiteralInteger (IntegerValueUint8 42))

  H.it "uint16 encodes to ValueChar (no dedicated GraphSON uint16 slot; uint32's slot is used)" $ do
    assertValueTag isChar (LiteralInteger (IntegerValueUint16 42))

  H.it "uint32 encodes to ValueChar (the modeled uint32 slot)" $ do
    assertValueTag isChar (LiteralInteger (IntegerValueUint32 42))

  H.it "uint64 encodes to ValueBigInteger (no dedicated GraphSON uint64 slot)" $ do
    assertValueTag isBigInteger (LiteralInteger (IntegerValueUint64 42))

  H.it "decimal encodes to ValueBigDecimal (previously unhandled, hard-errored)" $ do
    assertValueTag isBigDecimal (LiteralDecimal 42.0)

  where
    assertValueTag :: (Syntax.Value -> Bool) -> Literal -> H.Expectation
    assertValueTag isExpectedTag lit =
      case mapError $ GraphsonUtils.encodeLiteralValue lit of
        Left e -> H.expectationFailure $ "encodeLiteralValue failed: " ++ e
        Right v -> isExpectedTag v `H.shouldBe` True

    isBigInteger (Syntax.ValueBigInteger _) = True
    isBigInteger _ = False

    isByte (Syntax.ValueByte _) = True
    isByte _ = False

    isShort (Syntax.ValueShort _) = True
    isShort _ = False

    isChar (Syntax.ValueChar _) = True
    isChar _ = False

    isInteger (Syntax.ValueInteger _) = True
    isInteger _ = False

    isLong (Syntax.ValueLong _) = True
    isLong _ = False

    isBigDecimal (Syntax.ValueBigDecimal _) = True
    isBigDecimal _ = False

-- | #753: valueToJson used to emit every extended GraphSON type under the core "g:" namespace
-- instead of "gx:" (BigDecimal, BigInteger, Byte, Char, Duration, Int16, and ByteBuffer, which
-- was also misnamed "Binary"), and used non-standard core-type names "g:Long" and "g:DateTime"
-- instead of the TinkerPop-registered "g:Int64" and "g:Date". This asserts the exact "@type"
-- wire string per value kind, which the #744 tests above do not -- they only check the
-- Syntax.Value constructor tag, not the JSON type name actually written to the wire.
typeNameSpec :: H.Spec
typeNameSpec = H.describe "Pg.Graphson.Coder.valueToJson GraphSON 3.0 @type conformance (#753)" $ do

  H.it "bigDecimal emits gx:BigDecimal" $ do
    assertTypeName "gx:BigDecimal" (Syntax.ValueBigDecimal (Syntax.BigDecimalValue "42.0"))

  H.it "bigInteger emits gx:BigInteger" $ do
    assertTypeName "gx:BigInteger" (Syntax.ValueBigInteger 42)

  H.it "binary emits gx:ByteBuffer (not g:Binary)" $ do
    assertTypeName "gx:ByteBuffer" (Syntax.ValueBinary "AAA=")

  H.it "byte emits gx:Byte" $ do
    assertTypeName "gx:Byte" (Syntax.ValueByte 42)

  H.it "char emits gx:Char" $ do
    assertTypeName "gx:Char" (Syntax.ValueChar 65)

  H.it "dateTime emits g:Date (not g:DateTime, which is not a GraphSON 3.0 type)" $ do
    assertTypeName "g:Date" (Syntax.ValueDateTime (Syntax.DateTime "2026-10-02T00:00:00Z"))

  H.it "duration emits gx:Duration" $ do
    assertTypeName "gx:Duration" (Syntax.ValueDuration (Syntax.Duration "PT1H"))

  H.it "long (int64) emits g:Int64 (not g:Long)" $ do
    assertTypeName "g:Int64" (Syntax.ValueLong 42)

  H.it "short (int16) emits gx:Int16 (name was already correct; only the prefix was wrong)" $ do
    assertTypeName "gx:Int16" (Syntax.ValueShort 42)

  H.it "integer (int32) emits g:Int32 (already correct, unchanged)" $ do
    assertTypeName "g:Int32" (Syntax.ValueInteger 42)

  H.it "double emits g:Double (already correct, unchanged)" $ do
    assertTypeName "g:Double" (Syntax.ValueDouble (Syntax.DoubleValueFinite 4.2))

  H.it "float emits g:Float (already correct, unchanged)" $ do
    assertTypeName "g:Float" (Syntax.ValueFloat (Syntax.FloatValueFinite 4.2))

  H.it "uuid emits g:UUID (already correct, unchanged)" $ do
    assertTypeName "g:UUID" (Syntax.ValueUuid (Syntax.Uuid "123e4567-e89b-12d3-a456-426614174000"))

  H.it "list emits g:List (already correct, unchanged)" $ do
    assertTypeName "g:List" (Syntax.ValueList [])

  H.it "map emits g:Map (already correct, unchanged)" $ do
    assertTypeName "g:Map" (Syntax.ValueMap (Syntax.Map []))

  H.it "set emits g:Set (already correct, unchanged)" $ do
    assertTypeName "g:Set" (Syntax.ValueSet [])

  where
    assertTypeName :: String -> Syntax.Value -> H.Expectation
    assertTypeName expected v = case GraphsonCoder.valueToJson v of
      Model.ValueObject pairs -> case lookup "@type" pairs of
        Just (Model.ValueString actual) -> actual `H.shouldBe` expected
        _ -> H.expectationFailure "expected an \"@type\" string field"
      _ -> H.expectationFailure "expected a JSON object with \"@type\" and \"@value\" fields"
