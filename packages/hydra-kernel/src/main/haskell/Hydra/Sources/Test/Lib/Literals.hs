module Hydra.Sources.Test.Lib.Literals where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Literals as DefLiterals


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model", ModuleName "hydra.core.model", ModuleName "hydra.core.errors", ModuleName "hydra.core.test.testGraph", ModuleName "hydra.core.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.literals primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.core.lib.literals primitives
-- Note: Testing a representative subset of the many literal conversion functions

literalsBigintToInt32 :: TypedTerm TestGroup
literalsBigintToInt32 = supergroup "bigintToInt32" [
  subgroup "convertsInRange" [
    test "positive" "convertsInRange" 42 42,
    test "negative" "convertsInRange" (-42) (-42),
    test "zero" "convertsInRange" 0 0],
  subgroup "wrapsTwosComplementOutOfRange" [
    -- Two's-complement narrowing (#745): Int32 is represented as a native Int
    -- on hosts where that's word-sized (e.g. 64-bit Haskell Int), so a naive
    -- `fromIntegral` never narrows -- these catch that regression.
    test "wraps at max int32 + 1" "wrapsTwosComplementOutOfRange" 2147483648 (-2147483648),
    test "wraps at -1 equivalent" "wrapsTwosComplementOutOfRange" 4294967295 (-1)]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToInt32 [bigint x] (int32 result)

literalsInt32ToBigint :: TypedTerm TestGroup
literalsInt32ToBigint = subgroup "losslessConversion" [
  test "positive (int32 to bigint)" "losslessConversion" 42 42,
  test "negative (int32 to bigint)" "losslessConversion" (-42) (-42),
  test "zero (int32 to bigint)" "losslessConversion" 0 0]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.int32ToBigint [int32 x] (bigint result)

-- Other integer to bigint conversions

literalsInt16ToBigint :: TypedTerm TestGroup
literalsInt16ToBigint = subgroup "losslessConversion" [
  test "positive (int16 to bigint)" "losslessConversion" 1000 1000,
  test "negative (int16 to bigint)" "losslessConversion" (-1000) (-1000)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.int16ToBigint [int16 x] (bigint result)

literalsInt64ToBigint :: TypedTerm TestGroup
literalsInt64ToBigint = subgroup "losslessConversion" [
  test "positive (int64 to bigint)" "losslessConversion" 1000000 1000000,
  test "negative (int64 to bigint)" "losslessConversion" (-1000000) (-1000000)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.int64ToBigint [int64 x] (bigint result)

literalsInt8ToBigint :: TypedTerm TestGroup
literalsInt8ToBigint = subgroup "losslessConversion" [
  test "positive (int8 to bigint)" "losslessConversion" 42 42,
  test "negative (int8 to bigint)" "losslessConversion" (-42) (-42),
  test "max value (int8 to bigint)" "losslessConversion" 127 127,
  test "min value (int8 to bigint)" "losslessConversion" (-128) (-128)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.int8ToBigint [int8 x] (bigint result)

-- Unsigned integer to bigint conversions

literalsUint16ToBigint :: TypedTerm TestGroup
literalsUint16ToBigint = subgroup "losslessConversion" [
  test "zero (uint16 to bigint)" "losslessConversion" 0 0,
  test "typical value (uint16 to bigint)" "losslessConversion" 1000 1000]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.uint16ToBigint [uint16 x] (bigint result)

literalsUint32ToBigint :: TypedTerm TestGroup
literalsUint32ToBigint = subgroup "losslessConversion" [
  test "zero (uint32 to bigint)" "losslessConversion" 0 0,
  test "typical value (uint32 to bigint)" "losslessConversion" 100000 100000]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.uint32ToBigint [uint32 x] (bigint result)

literalsUint64ToBigint :: TypedTerm TestGroup
literalsUint64ToBigint = subgroup "losslessConversion" [
  test "zero (uint64 to bigint)" "losslessConversion" 0 0,
  test "typical value (uint64 to bigint)" "losslessConversion" 1000000 1000000]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.uint64ToBigint [uint64 x] (bigint result)

literalsUint8ToBigint :: TypedTerm TestGroup
literalsUint8ToBigint = subgroup "losslessConversion" [
  test "zero (uint8 to bigint)" "losslessConversion" 0 0,
  test "max value (uint8 to bigint)" "losslessConversion" 255 255]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.uint8ToBigint [uint8 x] (bigint result)

-- Bigint to other integer conversions

literalsBigintToInt16 :: TypedTerm TestGroup
literalsBigintToInt16 = supergroup "bigintToInt16" [
  subgroup "convertsInRange" [
    test "positive" "convertsInRange" 1000 1000,
    test "negative" "convertsInRange" (-1000) (-1000)],
  subgroup "wrapsTwosComplementOutOfRange" [
    -- Two's-complement narrowing (#745): a naive host impl (identity, no
    -- narrowing) fails these.
    test "wraps at max int16 + 1" "wrapsTwosComplementOutOfRange" 32768 (-32768),
    test "wraps at -1 equivalent" "wrapsTwosComplementOutOfRange" 65535 (-1)]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToInt16 [bigint x] (int16 result)

literalsBigintToInt64 :: TypedTerm TestGroup
literalsBigintToInt64 = subgroup "convertsInRange" [
  test "positive" "convertsInRange" 1000000 1000000,
  test "negative" "convertsInRange" (-1000000) (-1000000)]
  -- No wrap-boundary case here (unlike int8/16/32): the genuine int64 boundary
  -- (2^63) exceeds 2^53, JS Number's exact-integer ceiling, and the TypeScript host
  -- represents int64 terms as a native `number` -- a pre-existing scope limitation
  -- independent of #745 (every other host uses a true 64-bit/bigint representation).
  -- A boundary value small enough to round-trip exactly in a JS number would be far
  -- too small to actually exercise int64 wraparound, so there is no value that both
  -- proves the fix and is representable on every host; the fix itself (Lib/Literals.hs
  -- bigintToInt64, and the equivalent per-host narrowing) is still applied uniformly.
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToInt64 [bigint x] (int64 result)

literalsBigintToInt8 :: TypedTerm TestGroup
literalsBigintToInt8 = supergroup "bigintToInt8" [
  subgroup "convertsInRange" [
    test "positive" "convertsInRange" 42 42,
    test "negative" "convertsInRange" (-42) (-42)],
  subgroup "wrapsTwosComplementOutOfRange" [
    -- Two's-complement narrowing (#745).
    test "wraps at max int8 + 1" "wrapsTwosComplementOutOfRange" 128 (-128),
    test "wraps at -1 equivalent" "wrapsTwosComplementOutOfRange" 255 (-1)]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToInt8 [bigint x] (int8 result)

literalsBigintToUint16 :: TypedTerm TestGroup
literalsBigintToUint16 = subgroup "convertsInRange" [
  test "zero (bigint to uint16)" "convertsInRange" 0 0,
  test "typical value (bigint to uint16)" "convertsInRange" 1000 1000]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToUint16 [bigint x] (uint16 result)

literalsBigintToUint32 :: TypedTerm TestGroup
literalsBigintToUint32 = subgroup "convertsInRange" [
  test "zero (bigint to uint32)" "convertsInRange" 0 0,
  test "typical value (bigint to uint32)" "convertsInRange" 100000 100000]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToUint32 [bigint x] (uint32 result)

literalsBigintToUint64 :: TypedTerm TestGroup
literalsBigintToUint64 = subgroup "convertsInRange" [
  test "zero (bigint to uint64)" "convertsInRange" 0 0,
  test "typical value (bigint to uint64)" "convertsInRange" 1000000 1000000]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToUint64 [bigint x] (uint64 result)

literalsBigintToUint8 :: TypedTerm TestGroup
literalsBigintToUint8 = subgroup "convertsInRange" [
  test "zero (bigint to uint8)" "convertsInRange" 0 0,
  test "typical value (bigint to uint8)" "convertsInRange" 100 100]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToUint8 [bigint x] (uint8 result)

-- Float conversions

literalsFloat32ToFloat64 :: TypedTerm TestGroup
literalsFloat32ToFloat64 = subgroup "losslessWidening" [
  test "positive" "losslessWidening" 2.5 2.5,  -- exact in float32 and float64
  test "negative" "losslessWidening" (-2.5) (-2.5),
  test "zero" "losslessWidening" 0.0 0.0]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.float32ToFloat64 [float32 x] (float64 result)

literalsFloat64ToFloat32 :: TypedTerm TestGroup
literalsFloat64ToFloat32 = subgroup "convertsExactlyRepresentableValues" [
  test "positive (float64 to float32)" "convertsExactlyRepresentableValues" 2.5 2.5,
  test "negative (float64 to float32)" "convertsExactlyRepresentableValues" (-2.5) (-2.5),
  test "zero (float64 to float32)" "convertsExactlyRepresentableValues" 0.0 0.0]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.float64ToFloat32 [float64 x] (float32 result)

-- Decimal conversions

literalsBigintToDecimal :: TypedTerm TestGroup
literalsBigintToDecimal = subgroup "losslessConversion" [
  test "positive (bigint to decimal)" "losslessConversion" 42 42,
  test "negative (bigint to decimal)" "losslessConversion" (-42) (-42),
  test "zero (bigint to decimal)" "losslessConversion" 0 0]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.bigintToDecimal [bigint x] (decimal result)

literalsDecimalToBigint :: TypedTerm TestGroup
literalsDecimalToBigint = supergroup "decimalToBigint" [
  subgroup "convertsWholeNumbers" [
    test "positive whole" "convertsWholeNumbers" 42 42,
    test "negative whole" "convertsWholeNumbers" (-42) (-42),
    test "zero" "convertsWholeNumbers" 0 0],
  subgroup "truncatesFractionTowardZero" [
    test "round down" "truncatesFractionTowardZero" 42.3 42,
    test "round up" "truncatesFractionTowardZero" 42.7 43]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.decimalToBigint [decimal x] (bigint result)

literalsDecimalToFloat32 :: TypedTerm TestGroup
literalsDecimalToFloat32 = subgroup "convertsExactlyRepresentableValues" [
  test "zero (decimal to float32)" "convertsExactlyRepresentableValues" 0 0.0,
  test "positive whole (decimal to float32)" "convertsExactlyRepresentableValues" 2 2.0,
  test "negative whole (decimal to float32)" "convertsExactlyRepresentableValues" (-2) (-2.0)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.decimalToFloat32 [decimal x] (float32 result)

literalsDecimalToFloat64 :: TypedTerm TestGroup
literalsDecimalToFloat64 = subgroup "convertsExactlyRepresentableValues" [
  test "zero (decimal to float64)" "convertsExactlyRepresentableValues" 0 0.0,
  test "positive whole (decimal to float64)" "convertsExactlyRepresentableValues" 2 2.0,
  test "negative whole (decimal to float64)" "convertsExactlyRepresentableValues" (-2) (-2.0)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.decimalToFloat64 [decimal x] (float64 result)

literalsFloat32ToDecimal :: TypedTerm TestGroup
literalsFloat32ToDecimal = subgroup "convertsExactlyRepresentableValues" [
  test "zero (float32 to decimal)" "convertsExactlyRepresentableValues" 0.0 0,
  test "positive whole (float32 to decimal)" "convertsExactlyRepresentableValues" 2.0 2,
  test "negative whole (float32 to decimal)" "convertsExactlyRepresentableValues" (-2.0) (-2)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.float32ToDecimal [float32 x] (decimal result)

literalsFloat64ToDecimal :: TypedTerm TestGroup
literalsFloat64ToDecimal = subgroup "convertsExactlyRepresentableValues" [
  test "zero (float64 to decimal)" "convertsExactlyRepresentableValues" 0.0 0,
  test "positive whole (float64 to decimal)" "convertsExactlyRepresentableValues" 2.0 2,
  test "negative whole (float64 to decimal)" "convertsExactlyRepresentableValues" (-2.0) (-2)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.float64ToDecimal [float64 x] (decimal result)

literalsParseDecimal :: TypedTerm TestGroup
literalsParseDecimal = supergroup "parseDecimal" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "3.14" 3.14,
    testJust "zero" "0" 0,
    testJust "negative" "-42" (-42)],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseDecimal [string x] (Core.termOptional $ just (decimal result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseDecimal [string x] (Core.termOptional nothing)

literalsPrintDecimal :: TypedTerm TestGroup
literalsPrintDecimal = subgroup "rendersCanonicalDecimal" [
  test "zero (decimal)" "rendersCanonicalDecimal" 0 "0",
  test "positive whole (decimal)" "rendersCanonicalDecimal" 42 "42",
  test "negative whole (decimal)" "rendersCanonicalDecimal" (-42) "-42",
  test "positive fraction" "rendersCanonicalDecimal" 3.14 "3.14",
  test "negative fraction" "rendersCanonicalDecimal" (-2.5) "-2.5"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printDecimal [decimal x] (string result)

-- Print functions

literalsPrintBigint :: TypedTerm TestGroup
literalsPrintBigint = subgroup "rendersCanonicalDecimal" [
  test "positive (bigint)" "rendersCanonicalDecimal" 42 "42",
  test "negative (bigint)" "rendersCanonicalDecimal" (-42) "-42",
  test "zero (bigint)" "rendersCanonicalDecimal" 0 "0"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printBigint [bigint x] (string result)

literalsPrintBoolean :: TypedTerm TestGroup
literalsPrintBoolean = subgroup "rendersLowercaseLiterals" [
  test "true" "rendersLowercaseLiterals" true "true",
  test "false" "rendersLowercaseLiterals" false "false"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printBoolean [x] (string result)

literalsPrintFloat32 :: TypedTerm TestGroup
literalsPrintFloat32 = subgroup "rendersRoundTrippableRepresentation" [
  test "positive (float32)" "rendersRoundTrippableRepresentation" 3.14 "3.14",
  test "negative (float32)" "rendersRoundTrippableRepresentation" (-2.5) "-2.5",
  test "zero (float32)" "rendersRoundTrippableRepresentation" 0.0 "0.0",
  test "small positive (float32)" "rendersRoundTrippableRepresentation" 0.05 "5.0e-2",
  test "small positive 2 (float32)" "rendersRoundTrippableRepresentation" 0.03 "3.0e-2",
  test "very small (float32)" "rendersRoundTrippableRepresentation" 0.001 "1.0e-3",
  test "normal decimal (float32)" "rendersRoundTrippableRepresentation" 0.1 "0.1"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printFloat32 [float32 x] (string result)

literalsPrintFloat64 :: TypedTerm TestGroup
literalsPrintFloat64 = subgroup "rendersRoundTrippableRepresentation" [
  test "positive (float64)" "rendersRoundTrippableRepresentation" 3.14159 "3.14159",
  test "zero (float64)" "rendersRoundTrippableRepresentation" 0.0 "0.0",
  test "small positive (float64)" "rendersRoundTrippableRepresentation" 0.05 "5.0e-2",
  test "small positive 2 (float64)" "rendersRoundTrippableRepresentation" 0.03 "3.0e-2",
  test "very small (float64)" "rendersRoundTrippableRepresentation" 0.001 "1.0e-3",
  test "normal decimal (float64)" "rendersRoundTrippableRepresentation" 0.1 "0.1"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printFloat64 [float64 x] (string result)

literalsPrintInt16 :: TypedTerm TestGroup
literalsPrintInt16 = subgroup "rendersCanonicalDecimal" [
  test "positive (int16)" "rendersCanonicalDecimal" 1000 "1000",
  test "negative (int16)" "rendersCanonicalDecimal" (-1000) "-1000"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printInt16 [int16 x] (string result)

literalsPrintInt32 :: TypedTerm TestGroup
literalsPrintInt32 = subgroup "rendersCanonicalDecimal" [
  test "positive (int32)" "rendersCanonicalDecimal" 42 "42",
  test "negative (int32)" "rendersCanonicalDecimal" (-42) "-42",
  test "zero (int32)" "rendersCanonicalDecimal" 0 "0"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printInt32 [int32 x] (string result)

literalsPrintInt64 :: TypedTerm TestGroup
literalsPrintInt64 = subgroup "rendersCanonicalDecimal" [
  test "positive (int64)" "rendersCanonicalDecimal" 1000000 "1000000",
  test "negative (int64)" "rendersCanonicalDecimal" (-1000000) "-1000000"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printInt64 [int64 x] (string result)

literalsPrintInt8 :: TypedTerm TestGroup
literalsPrintInt8 = subgroup "rendersCanonicalDecimal" [
  test "positive (int8)" "rendersCanonicalDecimal" 42 "42",
  test "negative (int8)" "rendersCanonicalDecimal" (-42) "-42"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printInt8 [int8 x] (string result)

literalsPrintString :: TypedTerm TestGroup
literalsPrintString = supergroup "printString" [
  subgroup "quotesAndPassesThroughPlainCharacters" [
    test "simple" "quotesAndPassesThroughPlainCharacters" "hello" "\"hello\"",
    test "empty" "quotesAndPassesThroughPlainCharacters" "" "\"\""],
  subgroup "escapesNonAsciiAsDecimalCodes" [
    test "latin accented" "escapesNonAsciiAsDecimalCodes" "caf\233" "\"caf\\233\"",
    test "greek lambda" "escapesNonAsciiAsDecimalCodes" "\955" "\"\\955\"",
    test "mixed ascii and non-ascii" "escapesNonAsciiAsDecimalCodes" "A\233B" "\"A\\233B\""],
  subgroup "escapesNamedControlCharacters" [
    test "tab" "escapesNamedControlCharacters" "\t" "\"\\t\"",
    test "newline" "escapesNamedControlCharacters" "\n" "\"\\n\"",
    test "carriage return" "escapesNamedControlCharacters" "\r" "\"\\r\"",
    test "backslash" "escapesNamedControlCharacters" "\\" "\"\\\\\"",
    test "double quote" "escapesNamedControlCharacters" "\"" "\"\\\"\"",
    test "null" "escapesNamedControlCharacters" "\0" "\"\\NUL\"",
    test "bell" "escapesNamedControlCharacters" "\a" "\"\\a\"",
    test "backspace" "escapesNamedControlCharacters" "\b" "\"\\b\"",
    test "form feed" "escapesNamedControlCharacters" "\f" "\"\\f\"",
    test "vertical tab" "escapesNamedControlCharacters" "\v" "\"\\v\"",
    test "delete" "escapesNamedControlCharacters" "\127" "\"\\DEL\""]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printString [string x] (string result)

literalsPrintUint16 :: TypedTerm TestGroup
literalsPrintUint16 = subgroup "rendersCanonicalDecimal" [
  test "zero (uint16)" "rendersCanonicalDecimal" 0 "0",
  test "typical value (uint16)" "rendersCanonicalDecimal" 1000 "1000"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printUint16 [uint16 x] (string result)

literalsPrintUint32 :: TypedTerm TestGroup
literalsPrintUint32 = subgroup "rendersCanonicalDecimal" [
  test "zero (uint32)" "rendersCanonicalDecimal" 0 "0",
  test "typical value (uint32)" "rendersCanonicalDecimal" 100000 "100000"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printUint32 [uint32 x] (string result)

literalsPrintUint64 :: TypedTerm TestGroup
literalsPrintUint64 = subgroup "rendersCanonicalDecimal" [
  test "zero (uint64)" "rendersCanonicalDecimal" 0 "0",
  test "typical value (uint64)" "rendersCanonicalDecimal" 1000000 "1000000"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printUint64 [uint64 x] (string result)

literalsPrintUint8 :: TypedTerm TestGroup
literalsPrintUint8 = subgroup "rendersCanonicalDecimal" [
  test "zero (uint8)" "rendersCanonicalDecimal" 0 "0",
  test "max value (uint8)" "rendersCanonicalDecimal" 255 "255"]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.printUint8 [uint8 x] (string result)

-- Parse functions

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.literals primitives" $
    supergroup "hydra.core.lib.literals primitives" [
      -- Bigint conversions
      literalsBigintToInt8,
      literalsBigintToInt16,
      literalsBigintToInt32,
      literalsBigintToInt64,
      literalsBigintToUint8,
      literalsBigintToUint16,
      literalsBigintToUint32,
      literalsBigintToUint64,
      -- To bigint conversions
      literalsInt8ToBigint,
      literalsInt16ToBigint,
      literalsInt32ToBigint,
      literalsInt64ToBigint,
      literalsUint8ToBigint,
      literalsUint16ToBigint,
      literalsUint32ToBigint,
      literalsUint64ToBigint,
      -- Float conversions
      literalsFloat32ToFloat64,
      literalsFloat64ToFloat32,
      -- Decimal conversions
      literalsBigintToDecimal,
      literalsDecimalToBigint,
      literalsDecimalToFloat32,
      literalsDecimalToFloat64,
      literalsFloat32ToDecimal,
      literalsFloat64ToDecimal,
      literalsPrintDecimal,
      literalsParseDecimal,
      -- Print functions
      literalsPrintInt8,
      literalsPrintInt16,
      literalsPrintInt32,
      literalsPrintInt64,
      literalsPrintUint8,
      literalsPrintUint16,
      literalsPrintUint32,
      literalsPrintUint64,
      literalsPrintBigint,
      literalsPrintFloat32,
      literalsPrintFloat64,
      literalsPrintBoolean,
      literalsPrintString,
      -- Parse functions
      literalsParseInt8,
      literalsParseInt16,
      literalsParseInt32,
      literalsParseInt64,
      literalsParseUint8,
      literalsParseUint16,
      literalsParseUint32,
      literalsParseUint64,
      literalsParseBigint,
      literalsParseFloat32,
      literalsParseFloat64,
      literalsParseBoolean,
      literalsParseString,
      -- Binary conversions
      literalsBase64ToBinary,
      literalsBinaryToBase64,
      literalsBinaryToBytes]

literalsBinaryToBase64 :: TypedTerm TestGroup
literalsBinaryToBase64 = subgroup "encodesStandardBase64" [
  test "simple binary" "encodesStandardBase64" (BC.pack "hello") "aGVsbG8=",
  test "empty binary" "encodesStandardBase64" B.empty ""]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.binaryToBase64 [binary x] (string result)

literalsBinaryToBytes :: TypedTerm TestGroup
literalsBinaryToBytes = subgroup "returnsUnsignedByteValues" [
  test "empty binary" "returnsUnsignedByteValues" B.empty [],
  test "simple binary" "returnsUnsignedByteValues" (BC.pack "ab") [97, 98],
  test "byte value above 127 stays unsigned" "returnsUnsignedByteValues" (B.pack [0xFF, 0x00, 0x80]) [255, 0, 128]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.binaryToBytes [binary x] (list (int32 <$> result))

literalsParseBigint :: TypedTerm TestGroup
literalsParseBigint = supergroup "parseBigint" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "42" 42,
    testJust "negative" "-42" (-42),
    testJust "zero" "0" 0,
    primCaseWithTagsAndProvisions "large" [] ["parsesValidLiterals"] DefLiterals.parseBigint
      [string "123456789012345678901234567890"] (Core.termOptional $ just (bigint 123456789012345678901234567890))],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseBigint [string x] (Core.termOptional $ just (bigint result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseBigint [string x] (Core.termOptional nothing)

-- Binary/String conversion
-- Note: binaryToBase64 and base64ToBinary use base64 encoding

literalsParseBoolean :: TypedTerm TestGroup
literalsParseBoolean = supergroup "parseBoolean" [
  subgroup "parsesExactLowercaseLiterals" [
    testJust "true" "true" true,
    testJust "false" "false" false],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "yes"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesExactLowercaseLiterals"] DefLiterals.parseBoolean [string x] (Core.termOptional $ just result)
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseBoolean [string x] (Core.termOptional nothing)

literalsParseFloat32 :: TypedTerm TestGroup
literalsParseFloat32 = supergroup "parseFloat32" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "3.14" 3.14,
    testJust "negative" "-2.5" (-2.5)],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseFloat32 [string x] (Core.termOptional $ just (float32 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseFloat32 [string x] (Core.termOptional nothing)

literalsParseFloat64 :: TypedTerm TestGroup
literalsParseFloat64 = supergroup "parseFloat64" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "3.14159" 3.14159,
    testJust "negative" "-2.71828" (-2.71828)],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseFloat64 [string x] (Core.termOptional $ just (float64 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseFloat64 [string x] (Core.termOptional nothing)

literalsParseInt16 :: TypedTerm TestGroup
literalsParseInt16 = supergroup "parseInt16" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "1000" 1000,
    testJust "negative" "-1000" (-1000)],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseInt16 [string x] (Core.termOptional $ just (int16 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseInt16 [string x] (Core.termOptional nothing)

literalsParseInt32 :: TypedTerm TestGroup
literalsParseInt32 = supergroup "parseInt32" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "42" 42,
    testJust "negative" "-42" (-42)],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseInt32 [string x] (Core.termOptional $ just (int32 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseInt32 [string x] (Core.termOptional nothing)

literalsParseInt64 :: TypedTerm TestGroup
literalsParseInt64 = supergroup "parseInt64" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "1000000" 1000000,
    testJust "negative" "-1000000" (-1000000)],
  subgroup "noneOnInvalidInput" [
    testNothing "invalid" "abc"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseInt64 [string x] (Core.termOptional $ just (int64 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidInput"] DefLiterals.parseInt64 [string x] (Core.termOptional nothing)

literalsParseInt8 :: TypedTerm TestGroup
literalsParseInt8 = supergroup "parseInt8" [
  subgroup "parsesValidLiterals" [
    testJust "positive" "42" 42,
    testJust "negative" "-42" (-42),
    testJust "max value" "127" 127,
    testJust "min value" "-128" (-128)],
  subgroup "noneOnOutOfRangeOrInvalidInput" [
    testNothing "invalid" "abc",
    testNothing "overflow" "128"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseInt8 [string x] (Core.termOptional $ just (int8 result))
    testNothing name x = primCaseWithProvisions name ["noneOnOutOfRangeOrInvalidInput"] DefLiterals.parseInt8 [string x] (Core.termOptional nothing)

literalsParseString :: TypedTerm TestGroup
literalsParseString = supergroup "parseString" [
  subgroup "parsesQuotedLiterals" [
    testJust "quoted string" "\"hello\"" "hello",
    testJust "empty quoted" "\"\"" ""],
  subgroup "noneOnUnquotedInput" [
    testNothing "unquoted" "hello"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesQuotedLiterals"] DefLiterals.parseString [string x] (Core.termOptional $ just (string result))
    testNothing name x = primCaseWithProvisions name ["noneOnUnquotedInput"] DefLiterals.parseString [string x] (Core.termOptional nothing)

literalsParseUint16 :: TypedTerm TestGroup
literalsParseUint16 = supergroup "parseUint16" [
  subgroup "parsesValidLiterals" [
    testJust "zero" "0" 0,
    testJust "typical" "1000" 1000],
  subgroup "noneOnInvalidOrNegativeInput" [
    testNothing "invalid" "abc",
    testNothing "negative" "-1"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseUint16 [string x] (Core.termOptional $ just (uint16 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidOrNegativeInput"] DefLiterals.parseUint16 [string x] (Core.termOptional nothing)

literalsParseUint32 :: TypedTerm TestGroup
literalsParseUint32 = supergroup "parseUint32" [
  subgroup "parsesValidLiterals" [
    testJust "zero" "0" 0,
    testJust "typical" "100000" 100000,
    -- Upper-bound check (#745): a naive impl that only checks >= 0 accepts an
    -- out-of-range value instead of rejecting it.
    testJust "max value" "4294967295" 4294967295],
  subgroup "noneOnInvalidOrNegativeInput" [
    testNothing "invalid" "abc",
    testNothing "negative" "-1"],
  subgroup "noneOnOverflow" [
    testNothing "overflow" "4294967296"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseUint32 [string x] (Core.termOptional $ just (uint32 result))
    testNothing name x = primCaseWithProvisions name [segmentOf name] DefLiterals.parseUint32 [string x] (Core.termOptional nothing)
    segmentOf n = if n == "overflow" then "noneOnOverflow" else "noneOnInvalidOrNegativeInput"

literalsParseUint64 :: TypedTerm TestGroup
literalsParseUint64 = supergroup "parseUint64" [
  subgroup "parsesValidLiterals" [
    testJust "zero" "0" 0,
    testJust "typical" "1000000" 1000000,
    -- Upper-bound check (#745). "large in-range value" is well within 2^53 (JS Number's
    -- exact-integer ceiling) rather than the genuine uint64 max: the TypeScript host
    -- represents uint64 terms as a native `number` (a pre-existing scope limitation
    -- independent of #745), so the true max (2^64-1) cannot round-trip exactly there.
    -- The overflow-rejection case below doesn't depend on exact-value round-tripping,
    -- so it still exercises the true boundary (one past the real uint64 max).
    testJust "large in-range value" "9007199254740991" 9007199254740991],
  subgroup "noneOnInvalidOrNegativeInput" [
    testNothing "invalid" "abc",
    testNothing "negative" "-1"],
  subgroup "noneOnOverflow" [
    testNothing "overflow" "18446744073709551616"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseUint64 [string x] (Core.termOptional $ just (uint64 result))
    testNothing name x = primCaseWithProvisions name [segmentOf name] DefLiterals.parseUint64 [string x] (Core.termOptional nothing)
    segmentOf n = if n == "overflow" then "noneOnOverflow" else "noneOnInvalidOrNegativeInput"

literalsParseUint8 :: TypedTerm TestGroup
literalsParseUint8 = supergroup "parseUint8" [
  subgroup "parsesValidLiterals" [
    testJust "zero" "0" 0,
    testJust "typical" "100" 100,
    testJust "max value" "255" 255],
  subgroup "noneOnInvalidOrNegativeInput" [
    testNothing "invalid" "abc",
    testNothing "negative" "-1"]]
  where
    testJust name x result = primCaseWithProvisions name ["parsesValidLiterals"] DefLiterals.parseUint8 [string x] (Core.termOptional $ just (uint8 result))
    testNothing name x = primCaseWithProvisions name ["noneOnInvalidOrNegativeInput"] DefLiterals.parseUint8 [string x] (Core.termOptional nothing)

literalsBase64ToBinary :: TypedTerm TestGroup
literalsBase64ToBinary = subgroup "decodesStandardBase64" [
  test "simple base64" "decodesStandardBase64" "aGVsbG8=" (BC.pack "hello"),
  test "empty string" "decodesStandardBase64" "" B.empty]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefLiterals.base64ToBinary [string x] (binary result)
