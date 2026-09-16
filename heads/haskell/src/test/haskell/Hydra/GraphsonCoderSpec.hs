{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.GraphsonCoderSpec.spec
-}

module Hydra.GraphsonCoderSpec where

import Hydra.Kernel
import qualified Hydra.Pg.Graphson.Utils as GraphsonUtils
import qualified Hydra.Pg.Graphson.Syntax as Syntax

import Hydra.TestUtils

import qualified Test.Hspec as H


-- | #744: encodeLiteralValue used to handle only bigint/int32/int64 (of 8 integer widths) and
-- had no case for decimal, despite the GraphSON Value model having a dedicated slot for each
-- (byte/short/char/bigInteger/bigDecimal). Every width and decimal now encodes without error,
-- landing in the modeled slot for its width. This only checks that encoding succeeds and lands
-- on the correct Value constructor tag -- it deliberately does not assert exact native payload
-- values, since int8/uint16 widen through an intermediate width whose exact native Haskell type
-- is an implementation detail of the generic Hydra-to-Haskell integer mapping, not of this fix.
spec :: H.Spec
spec = H.describe "Pg.Graphson.Utils.encodeLiteralValue integer-width and decimal coverage (#744)" $ do

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
