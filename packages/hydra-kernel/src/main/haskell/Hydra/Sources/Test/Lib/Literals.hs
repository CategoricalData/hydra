module Hydra.Sources.Test.Lib.Literals where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Literals as DefLiterals


ns :: ModuleName
ns = ModuleName "hydra.test.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.print.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.literals primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.literals primitives
-- Note: Testing a representative subset of the many literal conversion functions

literalsBigintToInt32 :: TypedTerm TestGroup
literalsBigintToInt32 = subgroup "bigintToInt32" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name DefLiterals.bigintToInt32 [bigint x] (int32 result)

literalsInt32ToBigint :: TypedTerm TestGroup
literalsInt32ToBigint = subgroup "int32ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name DefLiterals.int32ToBigint [int32 x] (bigint result)

-- Other integer to bigint conversions

literalsInt16ToBigint :: TypedTerm TestGroup
literalsInt16ToBigint = subgroup "int16ToBigint" [
  test "positive" 1000 1000,
  test "negative" (-1000) (-1000)]
  where
    test name x result = primCase name DefLiterals.int16ToBigint [int16 x] (bigint result)

literalsInt64ToBigint :: TypedTerm TestGroup
literalsInt64ToBigint = subgroup "int64ToBigint" [
  test "positive" 1000000 1000000,
  test "negative" (-1000000) (-1000000)]
  where
    test name x result = primCase name DefLiterals.int64ToBigint [int64 x] (bigint result)

literalsInt8ToBigint :: TypedTerm TestGroup
literalsInt8ToBigint = subgroup "int8ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "max value" 127 127,
  test "min value" (-128) (-128)]
  where
    test name x result = primCase name DefLiterals.int8ToBigint [int8 x] (bigint result)

-- Unsigned integer to bigint conversions

literalsUint16ToBigint :: TypedTerm TestGroup
literalsUint16ToBigint = subgroup "uint16ToBigint" [
  test "zero" 0 0,
  test "typical value" 1000 1000]
  where
    test name x result = primCase name DefLiterals.uint16ToBigint [uint16 x] (bigint result)

literalsUint32ToBigint :: TypedTerm TestGroup
literalsUint32ToBigint = subgroup "uint32ToBigint" [
  test "zero" 0 0,
  test "typical value" 100000 100000]
  where
    test name x result = primCase name DefLiterals.uint32ToBigint [uint32 x] (bigint result)

literalsUint64ToBigint :: TypedTerm TestGroup
literalsUint64ToBigint = subgroup "uint64ToBigint" [
  test "zero" 0 0,
  test "typical value" 1000000 1000000]
  where
    test name x result = primCase name DefLiterals.uint64ToBigint [uint64 x] (bigint result)

literalsUint8ToBigint :: TypedTerm TestGroup
literalsUint8ToBigint = subgroup "uint8ToBigint" [
  test "zero" 0 0,
  test "max value" 255 255]
  where
    test name x result = primCase name DefLiterals.uint8ToBigint [uint8 x] (bigint result)

-- Bigint to other integer conversions

literalsBigintToInt16 :: TypedTerm TestGroup
literalsBigintToInt16 = subgroup "bigintToInt16" [
  test "positive" 1000 1000,
  test "negative" (-1000) (-1000)]
  where
    test name x result = primCase name DefLiterals.bigintToInt16 [bigint x] (int16 result)

literalsBigintToInt64 :: TypedTerm TestGroup
literalsBigintToInt64 = subgroup "bigintToInt64" [
  test "positive" 1000000 1000000,
  test "negative" (-1000000) (-1000000)]
  where
    test name x result = primCase name DefLiterals.bigintToInt64 [bigint x] (int64 result)

literalsBigintToInt8 :: TypedTerm TestGroup
literalsBigintToInt8 = subgroup "bigintToInt8" [
  test "positive" 42 42,
  test "negative" (-42) (-42)]
  where
    test name x result = primCase name DefLiterals.bigintToInt8 [bigint x] (int8 result)

literalsBigintToUint16 :: TypedTerm TestGroup
literalsBigintToUint16 = subgroup "bigintToUint16" [
  test "zero" 0 0,
  test "typical value" 1000 1000]
  where
    test name x result = primCase name DefLiterals.bigintToUint16 [bigint x] (uint16 result)

literalsBigintToUint32 :: TypedTerm TestGroup
literalsBigintToUint32 = subgroup "bigintToUint32" [
  test "zero" 0 0,
  test "typical value" 100000 100000]
  where
    test name x result = primCase name DefLiterals.bigintToUint32 [bigint x] (uint32 result)

literalsBigintToUint64 :: TypedTerm TestGroup
literalsBigintToUint64 = subgroup "bigintToUint64" [
  test "zero" 0 0,
  test "typical value" 1000000 1000000]
  where
    test name x result = primCase name DefLiterals.bigintToUint64 [bigint x] (uint64 result)

literalsBigintToUint8 :: TypedTerm TestGroup
literalsBigintToUint8 = subgroup "bigintToUint8" [
  test "zero" 0 0,
  test "typical value" 100 100]
  where
    test name x result = primCase name DefLiterals.bigintToUint8 [bigint x] (uint8 result)

-- Float conversions

literalsFloat32ToFloat64 :: TypedTerm TestGroup
literalsFloat32ToFloat64 = subgroup "float32ToFloat64" [
  test "positive" 2.5 2.5,  -- exact in float32 and float64
  test "negative" (-2.5) (-2.5),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name DefLiterals.float32ToFloat64 [float32 x] (float64 result)

literalsFloat64ToFloat32 :: TypedTerm TestGroup
literalsFloat64ToFloat32 = subgroup "float64ToFloat32" [
  test "positive" 2.5 2.5,
  test "negative" (-2.5) (-2.5),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name DefLiterals.float64ToFloat32 [float64 x] (float32 result)

-- Decimal conversions

literalsBigintToDecimal :: TypedTerm TestGroup
literalsBigintToDecimal = subgroup "bigintToDecimal" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name DefLiterals.bigintToDecimal [bigint x] (decimal result)

literalsDecimalToBigint :: TypedTerm TestGroup
literalsDecimalToBigint = subgroup "decimalToBigint" [
  test "positive whole" 42 42,
  test "negative whole" (-42) (-42),
  test "zero" 0 0,
  test "round down" 42.3 42,
  test "round up" 42.7 43]
  where
    test name x result = primCase name DefLiterals.decimalToBigint [decimal x] (bigint result)

literalsDecimalToFloat32 :: TypedTerm TestGroup
literalsDecimalToFloat32 = subgroup "decimalToFloat32" [
  test "zero" 0 0.0,
  test "positive whole" 2 2.0,
  test "negative whole" (-2) (-2.0)]
  where
    test name x result = primCase name DefLiterals.decimalToFloat32 [decimal x] (float32 result)

literalsDecimalToFloat64 :: TypedTerm TestGroup
literalsDecimalToFloat64 = subgroup "decimalToFloat64" [
  test "zero" 0 0.0,
  test "positive whole" 2 2.0,
  test "negative whole" (-2) (-2.0)]
  where
    test name x result = primCase name DefLiterals.decimalToFloat64 [decimal x] (float64 result)

literalsFloat32ToDecimal :: TypedTerm TestGroup
literalsFloat32ToDecimal = subgroup "float32ToDecimal" [
  test "zero" 0.0 0,
  test "positive whole" 2.0 2,
  test "negative whole" (-2.0) (-2)]
  where
    test name x result = primCase name DefLiterals.float32ToDecimal [float32 x] (decimal result)

literalsFloat64ToDecimal :: TypedTerm TestGroup
literalsFloat64ToDecimal = subgroup "float64ToDecimal" [
  test "zero" 0.0 0,
  test "positive whole" 2.0 2,
  test "negative whole" (-2.0) (-2)]
  where
    test name x result = primCase name DefLiterals.float64ToDecimal [float64 x] (decimal result)

literalsParseDecimal :: TypedTerm TestGroup
literalsParseDecimal = subgroup "parseDecimal" [
  testJust "positive" "3.14" 3.14,
  testJust "zero" "0" 0,
  testJust "negative" "-42" (-42),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseDecimal [string x] (Core.termOptional $ just (decimal result))
    testNothing name x = primCase name DefLiterals.parseDecimal [string x] (Core.termOptional nothing)

literalsPrintDecimal :: TypedTerm TestGroup
literalsPrintDecimal = subgroup "printDecimal" [
  test "zero" 0 "0.0",
  test "positive whole" 42 "42.0",
  test "negative whole" (-42) "-42.0",
  test "positive fraction" 3.14 "3.14",
  test "negative fraction" (-2.5) "-2.5"]
  where
    test name x result = primCase name DefLiterals.printDecimal [decimal x] (string result)

-- Print functions

literalsPrintBigint :: TypedTerm TestGroup
literalsPrintBigint = subgroup "printBigint" [
  test "positive" 42 "42",
  test "negative" (-42) "-42",
  test "zero" 0 "0"]
  where
    test name x result = primCase name DefLiterals.printBigint [bigint x] (string result)

literalsPrintBoolean :: TypedTerm TestGroup
literalsPrintBoolean = subgroup "printBoolean" [
  test "true" true "true",
  test "false" false "false"]
  where
    test name x result = primCase name DefLiterals.printBoolean [x] (string result)

literalsPrintFloat32 :: TypedTerm TestGroup
literalsPrintFloat32 = subgroup "printFloat32" [
  test "positive" 3.14 "3.14",
  test "negative" (-2.5) "-2.5",
  test "zero" 0.0 "0.0",
  test "small positive" 0.05 "5.0e-2",
  test "small positive 2" 0.03 "3.0e-2",
  test "very small" 0.001 "1.0e-3",
  test "normal decimal" 0.1 "0.1"]
  where
    test name x result = primCase name DefLiterals.printFloat32 [float32 x] (string result)

literalsPrintFloat64 :: TypedTerm TestGroup
literalsPrintFloat64 = subgroup "printFloat64" [
  test "positive" 3.14159 "3.14159",
  test "zero" 0.0 "0.0",
  test "small positive" 0.05 "5.0e-2",
  test "small positive 2" 0.03 "3.0e-2",
  test "very small" 0.001 "1.0e-3",
  test "normal decimal" 0.1 "0.1"]
  where
    test name x result = primCase name DefLiterals.printFloat64 [float64 x] (string result)

literalsPrintInt16 :: TypedTerm TestGroup
literalsPrintInt16 = subgroup "printInt16" [
  test "positive" 1000 "1000",
  test "negative" (-1000) "-1000"]
  where
    test name x result = primCase name DefLiterals.printInt16 [int16 x] (string result)

literalsPrintInt32 :: TypedTerm TestGroup
literalsPrintInt32 = subgroup "printInt32" [
  test "positive" 42 "42",
  test "negative" (-42) "-42",
  test "zero" 0 "0"]
  where
    test name x result = primCase name DefLiterals.printInt32 [int32 x] (string result)

literalsPrintInt64 :: TypedTerm TestGroup
literalsPrintInt64 = subgroup "printInt64" [
  test "positive" 1000000 "1000000",
  test "negative" (-1000000) "-1000000"]
  where
    test name x result = primCase name DefLiterals.printInt64 [int64 x] (string result)

literalsPrintInt8 :: TypedTerm TestGroup
literalsPrintInt8 = subgroup "printInt8" [
  test "positive" 42 "42",
  test "negative" (-42) "-42"]
  where
    test name x result = primCase name DefLiterals.printInt8 [int8 x] (string result)

literalsPrintString :: TypedTerm TestGroup
literalsPrintString = subgroup "printString" [
  test "simple" "hello" "\"hello\"",
  test "empty" "" "\"\"",
  -- Non-ASCII characters are escaped as decimal codes
  test "latin accented" "caf\233" "\"caf\\233\"",
  test "greek lambda" "\955" "\"\\955\"",
  test "mixed ascii and non-ascii" "A\233B" "\"A\\233B\"",
  -- Standard named escapes
  test "tab" "\t" "\"\\t\"",
  test "newline" "\n" "\"\\n\"",
  test "carriage return" "\r" "\"\\r\"",
  test "backslash" "\\" "\"\\\\\"",
  test "double quote" "\"" "\"\\\"\"",
  -- Control characters with named escapes
  test "null" "\0" "\"\\NUL\"",
  test "bell" "\a" "\"\\a\"",
  test "backspace" "\b" "\"\\b\"",
  test "form feed" "\f" "\"\\f\"",
  test "vertical tab" "\v" "\"\\v\"",
  test "delete" "\127" "\"\\DEL\""]
  where
    test name x result = primCase name DefLiterals.printString [string x] (string result)

literalsPrintUint16 :: TypedTerm TestGroup
literalsPrintUint16 = subgroup "printUint16" [
  test "zero" 0 "0",
  test "typical value" 1000 "1000"]
  where
    test name x result = primCase name DefLiterals.printUint16 [uint16 x] (string result)

literalsPrintUint32 :: TypedTerm TestGroup
literalsPrintUint32 = subgroup "printUint32" [
  test "zero" 0 "0",
  test "typical value" 100000 "100000"]
  where
    test name x result = primCase name DefLiterals.printUint32 [uint32 x] (string result)

literalsPrintUint64 :: TypedTerm TestGroup
literalsPrintUint64 = subgroup "printUint64" [
  test "zero" 0 "0",
  test "typical value" 1000000 "1000000"]
  where
    test name x result = primCase name DefLiterals.printUint64 [uint64 x] (string result)

literalsPrintUint8 :: TypedTerm TestGroup
literalsPrintUint8 = subgroup "printUint8" [
  test "zero" 0 "0",
  test "max value" 255 "255"]
  where
    test name x result = primCase name DefLiterals.printUint8 [uint8 x] (string result)

-- Parse functions

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.literals primitives" $
    supergroup "hydra.lib.literals primitives" [
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
literalsBinaryToBase64 = subgroup "binaryToBase64" [
  test "simple binary" (BC.pack "hello") "aGVsbG8=",
  test "empty binary" B.empty ""]
  where
    test name x result = primCase name DefLiterals.binaryToBase64 [binary x] (string result)

literalsBinaryToBytes :: TypedTerm TestGroup
literalsBinaryToBytes = subgroup "binaryToBytes" [
  test "empty binary" B.empty [],
  test "simple binary" (BC.pack "ab") [97, 98],
  test "byte value above 127 stays unsigned" (B.pack [0xFF, 0x00, 0x80]) [255, 0, 128]]
  where
    test name x result = primCase name DefLiterals.binaryToBytes [binary x] (list (int32 <$> result))

literalsParseBigint :: TypedTerm TestGroup
literalsParseBigint = subgroup "parseBigint" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testJust "zero" "0" 0,
  primCaseWithTags "large" [] DefLiterals.parseBigint [string "123456789012345678901234567890"] (Core.termOptional $ just (bigint 123456789012345678901234567890)),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseBigint [string x] (Core.termOptional $ just (bigint result))
    testNothing name x = primCase name DefLiterals.parseBigint [string x] (Core.termOptional nothing)

-- Binary/String conversion
-- Note: binaryToBase64 and base64ToBinary use base64 encoding

literalsParseBoolean :: TypedTerm TestGroup
literalsParseBoolean = subgroup "parseBoolean" [
  testJust "true" "true" true,
  testJust "false" "false" false,
  testNothing "invalid" "yes"]
  where
    testJust name x result = primCase name DefLiterals.parseBoolean [string x] (Core.termOptional $ just result)
    testNothing name x = primCase name DefLiterals.parseBoolean [string x] (Core.termOptional nothing)

literalsParseFloat32 :: TypedTerm TestGroup
literalsParseFloat32 = subgroup "parseFloat32" [
  testJust "positive" "3.14" 3.14,
  testJust "negative" "-2.5" (-2.5),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseFloat32 [string x] (Core.termOptional $ just (float32 result))
    testNothing name x = primCase name DefLiterals.parseFloat32 [string x] (Core.termOptional nothing)

literalsParseFloat64 :: TypedTerm TestGroup
literalsParseFloat64 = subgroup "parseFloat64" [
  testJust "positive" "3.14159" 3.14159,
  testJust "negative" "-2.71828" (-2.71828),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseFloat64 [string x] (Core.termOptional $ just (float64 result))
    testNothing name x = primCase name DefLiterals.parseFloat64 [string x] (Core.termOptional nothing)

literalsParseInt16 :: TypedTerm TestGroup
literalsParseInt16 = subgroup "parseInt16" [
  testJust "positive" "1000" 1000,
  testJust "negative" "-1000" (-1000),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseInt16 [string x] (Core.termOptional $ just (int16 result))
    testNothing name x = primCase name DefLiterals.parseInt16 [string x] (Core.termOptional nothing)

literalsParseInt32 :: TypedTerm TestGroup
literalsParseInt32 = subgroup "parseInt32" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseInt32 [string x] (Core.termOptional $ just (int32 result))
    testNothing name x = primCase name DefLiterals.parseInt32 [string x] (Core.termOptional nothing)

literalsParseInt64 :: TypedTerm TestGroup
literalsParseInt64 = subgroup "parseInt64" [
  testJust "positive" "1000000" 1000000,
  testJust "negative" "-1000000" (-1000000),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name DefLiterals.parseInt64 [string x] (Core.termOptional $ just (int64 result))
    testNothing name x = primCase name DefLiterals.parseInt64 [string x] (Core.termOptional nothing)

literalsParseInt8 :: TypedTerm TestGroup
literalsParseInt8 = subgroup "parseInt8" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testJust "max value" "127" 127,
  testJust "min value" "-128" (-128),
  testNothing "invalid" "abc",
  testNothing "overflow" "128"]
  where
    testJust name x result = primCase name DefLiterals.parseInt8 [string x] (Core.termOptional $ just (int8 result))
    testNothing name x = primCase name DefLiterals.parseInt8 [string x] (Core.termOptional nothing)

literalsParseString :: TypedTerm TestGroup
literalsParseString = subgroup "parseString" [
  testJust "quoted string" "\"hello\"" "hello",
  testJust "empty quoted" "\"\"" "",
  testNothing "unquoted" "hello"]
  where
    testJust name x result = primCase name DefLiterals.parseString [string x] (Core.termOptional $ just (string result))
    testNothing name x = primCase name DefLiterals.parseString [string x] (Core.termOptional nothing)

literalsParseUint16 :: TypedTerm TestGroup
literalsParseUint16 = subgroup "parseUint16" [
  testJust "zero" "0" 0,
  testJust "typical" "1000" 1000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name DefLiterals.parseUint16 [string x] (Core.termOptional $ just (uint16 result))
    testNothing name x = primCase name DefLiterals.parseUint16 [string x] (Core.termOptional nothing)

literalsParseUint32 :: TypedTerm TestGroup
literalsParseUint32 = subgroup "parseUint32" [
  testJust "zero" "0" 0,
  testJust "typical" "100000" 100000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name DefLiterals.parseUint32 [string x] (Core.termOptional $ just (uint32 result))
    testNothing name x = primCase name DefLiterals.parseUint32 [string x] (Core.termOptional nothing)

literalsParseUint64 :: TypedTerm TestGroup
literalsParseUint64 = subgroup "parseUint64" [
  testJust "zero" "0" 0,
  testJust "typical" "1000000" 1000000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name DefLiterals.parseUint64 [string x] (Core.termOptional $ just (uint64 result))
    testNothing name x = primCase name DefLiterals.parseUint64 [string x] (Core.termOptional nothing)

literalsParseUint8 :: TypedTerm TestGroup
literalsParseUint8 = subgroup "parseUint8" [
  testJust "zero" "0" 0,
  testJust "typical" "100" 100,
  testJust "max value" "255" 255,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name DefLiterals.parseUint8 [string x] (Core.termOptional $ just (uint8 result))
    testNothing name x = primCase name DefLiterals.parseUint8 [string x] (Core.termOptional nothing)

literalsBase64ToBinary :: TypedTerm TestGroup
literalsBase64ToBinary = subgroup "base64ToBinary" [
  test "simple base64" "aGVsbG8=" (BC.pack "hello"),
  test "empty string" "" B.empty]
  where
    test name x result = primCase name DefLiterals.base64ToBinary [string x] (binary result)
