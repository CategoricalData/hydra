module Hydra.Sources.Test.Lib.Literals where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


ns :: ModuleName
ns = ModuleName "hydra.test.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.show.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing"],
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
    test name x result = primCase name _literals_bigintToInt32 [bigint x] (int32 result)

literalsInt32ToBigint :: TypedTerm TestGroup
literalsInt32ToBigint = subgroup "int32ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name _literals_int32ToBigint [int32 x] (bigint result)

-- Other integer to bigint conversions

literalsInt16ToBigint :: TypedTerm TestGroup
literalsInt16ToBigint = subgroup "int16ToBigint" [
  test "positive" 1000 1000,
  test "negative" (-1000) (-1000)]
  where
    test name x result = primCase name _literals_int16ToBigint [int16 x] (bigint result)

literalsInt64ToBigint :: TypedTerm TestGroup
literalsInt64ToBigint = subgroup "int64ToBigint" [
  test "positive" 1000000 1000000,
  test "negative" (-1000000) (-1000000)]
  where
    test name x result = primCase name _literals_int64ToBigint [int64 x] (bigint result)

literalsInt8ToBigint :: TypedTerm TestGroup
literalsInt8ToBigint = subgroup "int8ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "max value" 127 127,
  test "min value" (-128) (-128)]
  where
    test name x result = primCase name _literals_int8ToBigint [int8 x] (bigint result)

-- Unsigned integer to bigint conversions

literalsUint16ToBigint :: TypedTerm TestGroup
literalsUint16ToBigint = subgroup "uint16ToBigint" [
  test "zero" 0 0,
  test "typical value" 1000 1000]
  where
    test name x result = primCase name _literals_uint16ToBigint [uint16 x] (bigint result)

literalsUint32ToBigint :: TypedTerm TestGroup
literalsUint32ToBigint = subgroup "uint32ToBigint" [
  test "zero" 0 0,
  test "typical value" 100000 100000]
  where
    test name x result = primCase name _literals_uint32ToBigint [uint32 x] (bigint result)

literalsUint64ToBigint :: TypedTerm TestGroup
literalsUint64ToBigint = subgroup "uint64ToBigint" [
  test "zero" 0 0,
  test "typical value" 1000000 1000000]
  where
    test name x result = primCase name _literals_uint64ToBigint [uint64 x] (bigint result)

literalsUint8ToBigint :: TypedTerm TestGroup
literalsUint8ToBigint = subgroup "uint8ToBigint" [
  test "zero" 0 0,
  test "max value" 255 255]
  where
    test name x result = primCase name _literals_uint8ToBigint [uint8 x] (bigint result)

-- Bigint to other integer conversions

literalsBigintToInt16 :: TypedTerm TestGroup
literalsBigintToInt16 = subgroup "bigintToInt16" [
  test "positive" 1000 1000,
  test "negative" (-1000) (-1000)]
  where
    test name x result = primCase name _literals_bigintToInt16 [bigint x] (int16 result)

literalsBigintToInt64 :: TypedTerm TestGroup
literalsBigintToInt64 = subgroup "bigintToInt64" [
  test "positive" 1000000 1000000,
  test "negative" (-1000000) (-1000000)]
  where
    test name x result = primCase name _literals_bigintToInt64 [bigint x] (int64 result)

literalsBigintToInt8 :: TypedTerm TestGroup
literalsBigintToInt8 = subgroup "bigintToInt8" [
  test "positive" 42 42,
  test "negative" (-42) (-42)]
  where
    test name x result = primCase name _literals_bigintToInt8 [bigint x] (int8 result)

literalsBigintToUint16 :: TypedTerm TestGroup
literalsBigintToUint16 = subgroup "bigintToUint16" [
  test "zero" 0 0,
  test "typical value" 1000 1000]
  where
    test name x result = primCase name _literals_bigintToUint16 [bigint x] (uint16 result)

literalsBigintToUint32 :: TypedTerm TestGroup
literalsBigintToUint32 = subgroup "bigintToUint32" [
  test "zero" 0 0,
  test "typical value" 100000 100000]
  where
    test name x result = primCase name _literals_bigintToUint32 [bigint x] (uint32 result)

literalsBigintToUint64 :: TypedTerm TestGroup
literalsBigintToUint64 = subgroup "bigintToUint64" [
  test "zero" 0 0,
  test "typical value" 1000000 1000000]
  where
    test name x result = primCase name _literals_bigintToUint64 [bigint x] (uint64 result)

literalsBigintToUint8 :: TypedTerm TestGroup
literalsBigintToUint8 = subgroup "bigintToUint8" [
  test "zero" 0 0,
  test "typical value" 100 100]
  where
    test name x result = primCase name _literals_bigintToUint8 [bigint x] (uint8 result)

-- Float conversions

literalsFloat32ToFloat64 :: TypedTerm TestGroup
literalsFloat32ToFloat64 = subgroup "float32ToFloat64" [
  test "positive" 2.5 2.5,  -- exact in float32 and float64
  test "negative" (-2.5) (-2.5),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name _literals_float32ToFloat64 [float32 x] (float64 result)

literalsFloat64ToFloat32 :: TypedTerm TestGroup
literalsFloat64ToFloat32 = subgroup "float64ToFloat32" [
  test "positive" 2.5 2.5,
  test "negative" (-2.5) (-2.5),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name _literals_float64ToFloat32 [float64 x] (float32 result)

-- Decimal conversions

literalsBigintToDecimal :: TypedTerm TestGroup
literalsBigintToDecimal = subgroup "bigintToDecimal" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name _literals_bigintToDecimal [bigint x] (decimal result)

literalsDecimalToBigint :: TypedTerm TestGroup
literalsDecimalToBigint = subgroup "decimalToBigint" [
  test "positive whole" 42 42,
  test "negative whole" (-42) (-42),
  test "zero" 0 0,
  test "round down" 42.3 42,
  test "round up" 42.7 43]
  where
    test name x result = primCase name _literals_decimalToBigint [decimal x] (bigint result)

literalsDecimalToFloat32 :: TypedTerm TestGroup
literalsDecimalToFloat32 = subgroup "decimalToFloat32" [
  test "zero" 0 0.0,
  test "positive whole" 2 2.0,
  test "negative whole" (-2) (-2.0)]
  where
    test name x result = primCase name _literals_decimalToFloat32 [decimal x] (float32 result)

literalsDecimalToFloat64 :: TypedTerm TestGroup
literalsDecimalToFloat64 = subgroup "decimalToFloat64" [
  test "zero" 0 0.0,
  test "positive whole" 2 2.0,
  test "negative whole" (-2) (-2.0)]
  where
    test name x result = primCase name _literals_decimalToFloat64 [decimal x] (float64 result)

literalsFloat32ToDecimal :: TypedTerm TestGroup
literalsFloat32ToDecimal = subgroup "float32ToDecimal" [
  test "zero" 0.0 0,
  test "positive whole" 2.0 2,
  test "negative whole" (-2.0) (-2)]
  where
    test name x result = primCase name _literals_float32ToDecimal [float32 x] (decimal result)

literalsFloat64ToDecimal :: TypedTerm TestGroup
literalsFloat64ToDecimal = subgroup "float64ToDecimal" [
  test "zero" 0.0 0,
  test "positive whole" 2.0 2,
  test "negative whole" (-2.0) (-2)]
  where
    test name x result = primCase name _literals_float64ToDecimal [float64 x] (decimal result)

literalsReadDecimal :: TypedTerm TestGroup
literalsReadDecimal = subgroup "readDecimal" [
  testJust "positive" "3.14" 3.14,
  testJust "zero" "0" 0,
  testJust "negative" "-42" (-42),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readDecimal [string x] (Core.termMaybe $ just (decimal result))
    testNothing name x = primCase name _literals_readDecimal [string x] (Core.termMaybe nothing)

literalsShowDecimal :: TypedTerm TestGroup
literalsShowDecimal = subgroup "showDecimal" [
  test "zero" 0 "0.0",
  test "positive whole" 42 "42.0",
  test "negative whole" (-42) "-42.0",
  test "positive fraction" 3.14 "3.14",
  test "negative fraction" (-2.5) "-2.5"]
  where
    test name x result = primCase name _literals_showDecimal [decimal x] (string result)

-- Show functions

literalsShowBigint :: TypedTerm TestGroup
literalsShowBigint = subgroup "showBigint" [
  test "positive" 42 "42",
  test "negative" (-42) "-42",
  test "zero" 0 "0"]
  where
    test name x result = primCase name _literals_showBigint [bigint x] (string result)

literalsShowBoolean :: TypedTerm TestGroup
literalsShowBoolean = subgroup "showBoolean" [
  test "true" true "true",
  test "false" false "false"]
  where
    test name x result = primCase name _literals_showBoolean [x] (string result)

literalsShowFloat32 :: TypedTerm TestGroup
literalsShowFloat32 = subgroup "showFloat32" [
  test "positive" 3.14 "3.14",
  test "negative" (-2.5) "-2.5",
  test "zero" 0.0 "0.0",
  test "small positive" 0.05 "5.0e-2",
  test "small positive 2" 0.03 "3.0e-2",
  test "very small" 0.001 "1.0e-3",
  test "normal decimal" 0.1 "0.1"]
  where
    test name x result = primCase name _literals_showFloat32 [float32 x] (string result)

literalsShowFloat64 :: TypedTerm TestGroup
literalsShowFloat64 = subgroup "showFloat64" [
  test "positive" 3.14159 "3.14159",
  test "zero" 0.0 "0.0",
  test "small positive" 0.05 "5.0e-2",
  test "small positive 2" 0.03 "3.0e-2",
  test "very small" 0.001 "1.0e-3",
  test "normal decimal" 0.1 "0.1"]
  where
    test name x result = primCase name _literals_showFloat64 [float64 x] (string result)

literalsShowInt16 :: TypedTerm TestGroup
literalsShowInt16 = subgroup "showInt16" [
  test "positive" 1000 "1000",
  test "negative" (-1000) "-1000"]
  where
    test name x result = primCase name _literals_showInt16 [int16 x] (string result)

literalsShowInt32 :: TypedTerm TestGroup
literalsShowInt32 = subgroup "showInt32" [
  test "positive" 42 "42",
  test "negative" (-42) "-42",
  test "zero" 0 "0"]
  where
    test name x result = primCase name _literals_showInt32 [int32 x] (string result)

literalsShowInt64 :: TypedTerm TestGroup
literalsShowInt64 = subgroup "showInt64" [
  test "positive" 1000000 "1000000",
  test "negative" (-1000000) "-1000000"]
  where
    test name x result = primCase name _literals_showInt64 [int64 x] (string result)

literalsShowInt8 :: TypedTerm TestGroup
literalsShowInt8 = subgroup "showInt8" [
  test "positive" 42 "42",
  test "negative" (-42) "-42"]
  where
    test name x result = primCase name _literals_showInt8 [int8 x] (string result)

literalsShowString :: TypedTerm TestGroup
literalsShowString = subgroup "showString" [
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
    test name x result = primCase name _literals_showString [string x] (string result)

literalsShowUint16 :: TypedTerm TestGroup
literalsShowUint16 = subgroup "showUint16" [
  test "zero" 0 "0",
  test "typical value" 1000 "1000"]
  where
    test name x result = primCase name _literals_showUint16 [uint16 x] (string result)

literalsShowUint32 :: TypedTerm TestGroup
literalsShowUint32 = subgroup "showUint32" [
  test "zero" 0 "0",
  test "typical value" 100000 "100000"]
  where
    test name x result = primCase name _literals_showUint32 [uint32 x] (string result)

literalsShowUint64 :: TypedTerm TestGroup
literalsShowUint64 = subgroup "showUint64" [
  test "zero" 0 "0",
  test "typical value" 1000000 "1000000"]
  where
    test name x result = primCase name _literals_showUint64 [uint64 x] (string result)

literalsShowUint8 :: TypedTerm TestGroup
literalsShowUint8 = subgroup "showUint8" [
  test "zero" 0 "0",
  test "max value" 255 "255"]
  where
    test name x result = primCase name _literals_showUint8 [uint8 x] (string result)

-- Read functions

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
      literalsShowDecimal,
      literalsReadDecimal,
      -- Show functions
      literalsShowInt8,
      literalsShowInt16,
      literalsShowInt32,
      literalsShowInt64,
      literalsShowUint8,
      literalsShowUint16,
      literalsShowUint32,
      literalsShowUint64,
      literalsShowBigint,
      literalsShowFloat32,
      literalsShowFloat64,
      literalsShowBoolean,
      literalsShowString,
      -- Read functions
      literalsReadInt8,
      literalsReadInt16,
      literalsReadInt32,
      literalsReadInt64,
      literalsReadUint8,
      literalsReadUint16,
      literalsReadUint32,
      literalsReadUint64,
      literalsReadBigint,
      literalsReadFloat32,
      literalsReadFloat64,
      literalsReadBoolean,
      literalsReadString,
      -- Binary conversions
      literalsStringToBinary,
      literalsBinaryToString]

literalsBinaryToString :: TypedTerm TestGroup
literalsBinaryToString = subgroup "binaryToString" [
  test "simple binary" (BC.pack "hello") "aGVsbG8=",
  test "empty binary" B.empty ""]
  where
    test name x result = primCase name _literals_binaryToString [binary x] (string result)

literalsReadBigint :: TypedTerm TestGroup
literalsReadBigint = subgroup "readBigint" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testJust "zero" "0" 0,
  primCaseWithTags "large" [tag_disabled] _literals_readBigint [string "123456789012345678901234567890"] (Core.termMaybe $ just (bigint 123456789012345678901234567890)),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readBigint [string x] (Core.termMaybe $ just (bigint result))
    testNothing name x = primCase name _literals_readBigint [string x] (Core.termMaybe nothing)

-- Binary/String conversion
-- Note: binaryToString and stringToBinary use base64 encoding

literalsReadBoolean :: TypedTerm TestGroup
literalsReadBoolean = subgroup "readBoolean" [
  testJust "true" "true" true,
  testJust "false" "false" false,
  testNothing "invalid" "yes"]
  where
    testJust name x result = primCase name _literals_readBoolean [string x] (Core.termMaybe $ just result)
    testNothing name x = primCase name _literals_readBoolean [string x] (Core.termMaybe nothing)

literalsReadFloat32 :: TypedTerm TestGroup
literalsReadFloat32 = subgroup "readFloat32" [
  testJust "positive" "3.14" 3.14,
  testJust "negative" "-2.5" (-2.5),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readFloat32 [string x] (Core.termMaybe $ just (float32 result))
    testNothing name x = primCase name _literals_readFloat32 [string x] (Core.termMaybe nothing)

literalsReadFloat64 :: TypedTerm TestGroup
literalsReadFloat64 = subgroup "readFloat64" [
  testJust "positive" "3.14159" 3.14159,
  testJust "negative" "-2.71828" (-2.71828),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readFloat64 [string x] (Core.termMaybe $ just (float64 result))
    testNothing name x = primCase name _literals_readFloat64 [string x] (Core.termMaybe nothing)

literalsReadInt16 :: TypedTerm TestGroup
literalsReadInt16 = subgroup "readInt16" [
  testJust "positive" "1000" 1000,
  testJust "negative" "-1000" (-1000),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readInt16 [string x] (Core.termMaybe $ just (int16 result))
    testNothing name x = primCase name _literals_readInt16 [string x] (Core.termMaybe nothing)

literalsReadInt32 :: TypedTerm TestGroup
literalsReadInt32 = subgroup "readInt32" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readInt32 [string x] (Core.termMaybe $ just (int32 result))
    testNothing name x = primCase name _literals_readInt32 [string x] (Core.termMaybe nothing)

literalsReadInt64 :: TypedTerm TestGroup
literalsReadInt64 = subgroup "readInt64" [
  testJust "positive" "1000000" 1000000,
  testJust "negative" "-1000000" (-1000000),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readInt64 [string x] (Core.termMaybe $ just (int64 result))
    testNothing name x = primCase name _literals_readInt64 [string x] (Core.termMaybe nothing)

literalsReadInt8 :: TypedTerm TestGroup
literalsReadInt8 = subgroup "readInt8" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testJust "max value" "127" 127,
  testJust "min value" "-128" (-128),
  testNothing "invalid" "abc",
  testNothing "overflow" "128"]
  where
    testJust name x result = primCase name _literals_readInt8 [string x] (Core.termMaybe $ just (int8 result))
    testNothing name x = primCase name _literals_readInt8 [string x] (Core.termMaybe nothing)

literalsReadString :: TypedTerm TestGroup
literalsReadString = subgroup "readString" [
  testJust "quoted string" "\"hello\"" "hello",
  testJust "empty quoted" "\"\"" "",
  testNothing "unquoted" "hello"]
  where
    testJust name x result = primCase name _literals_readString [string x] (Core.termMaybe $ just (string result))
    testNothing name x = primCase name _literals_readString [string x] (Core.termMaybe nothing)

literalsReadUint16 :: TypedTerm TestGroup
literalsReadUint16 = subgroup "readUint16" [
  testJust "zero" "0" 0,
  testJust "typical" "1000" 1000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint16 [string x] (Core.termMaybe $ just (uint16 result))
    testNothing name x = primCase name _literals_readUint16 [string x] (Core.termMaybe nothing)

literalsReadUint32 :: TypedTerm TestGroup
literalsReadUint32 = subgroup "readUint32" [
  testJust "zero" "0" 0,
  testJust "typical" "100000" 100000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint32 [string x] (Core.termMaybe $ just (uint32 result))
    testNothing name x = primCase name _literals_readUint32 [string x] (Core.termMaybe nothing)

literalsReadUint64 :: TypedTerm TestGroup
literalsReadUint64 = subgroup "readUint64" [
  testJust "zero" "0" 0,
  testJust "typical" "1000000" 1000000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint64 [string x] (Core.termMaybe $ just (uint64 result))
    testNothing name x = primCase name _literals_readUint64 [string x] (Core.termMaybe nothing)

literalsReadUint8 :: TypedTerm TestGroup
literalsReadUint8 = subgroup "readUint8" [
  testJust "zero" "0" 0,
  testJust "typical" "100" 100,
  testJust "max value" "255" 255,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint8 [string x] (Core.termMaybe $ just (uint8 result))
    testNothing name x = primCase name _literals_readUint8 [string x] (Core.termMaybe nothing)

literalsStringToBinary :: TypedTerm TestGroup
literalsStringToBinary = subgroup "stringToBinary" [
  test "simple base64" "aGVsbG8=" (BC.pack "hello"),
  test "empty string" "" B.empty]
  where
    test name x result = primCase name _literals_stringToBinary [string x] (binary result)
