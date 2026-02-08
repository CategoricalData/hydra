module Hydra.Sources.Test.Lib.Literals where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
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


ns :: Namespace
ns = Namespace "hydra.test.lib.literals"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for hydra.lib.literals primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Test groups for hydra.lib.literals primitives
-- Note: Testing a representative subset of the many literal conversion functions

literalsBigintToInt32 :: TTerm TestGroup
literalsBigintToInt32 = subgroup "bigintToInt32" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name _literals_bigintToInt32 [bigint x] (int32 result)

literalsInt32ToBigint :: TTerm TestGroup
literalsInt32ToBigint = subgroup "int32ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name _literals_int32ToBigint [int32 x] (bigint result)

-- Other integer to bigint conversions

literalsInt8ToBigint :: TTerm TestGroup
literalsInt8ToBigint = subgroup "int8ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "max value" 127 127,
  test "min value" (-128) (-128)]
  where
    test name x result = primCase name _literals_int8ToBigint [int8 x] (bigint result)

literalsInt16ToBigint :: TTerm TestGroup
literalsInt16ToBigint = subgroup "int16ToBigint" [
  test "positive" 1000 1000,
  test "negative" (-1000) (-1000)]
  where
    test name x result = primCase name _literals_int16ToBigint [int16 x] (bigint result)

literalsInt64ToBigint :: TTerm TestGroup
literalsInt64ToBigint = subgroup "int64ToBigint" [
  test "positive" 1000000 1000000,
  test "negative" (-1000000) (-1000000)]
  where
    test name x result = primCase name _literals_int64ToBigint [int64 x] (bigint result)

-- Unsigned integer to bigint conversions

literalsUint8ToBigint :: TTerm TestGroup
literalsUint8ToBigint = subgroup "uint8ToBigint" [
  test "zero" 0 0,
  test "max value" 255 255]
  where
    test name x result = primCase name _literals_uint8ToBigint [uint8 x] (bigint result)

literalsUint16ToBigint :: TTerm TestGroup
literalsUint16ToBigint = subgroup "uint16ToBigint" [
  test "zero" 0 0,
  test "typical value" 1000 1000]
  where
    test name x result = primCase name _literals_uint16ToBigint [uint16 x] (bigint result)

literalsUint32ToBigint :: TTerm TestGroup
literalsUint32ToBigint = subgroup "uint32ToBigint" [
  test "zero" 0 0,
  test "typical value" 100000 100000]
  where
    test name x result = primCase name _literals_uint32ToBigint [uint32 x] (bigint result)

literalsUint64ToBigint :: TTerm TestGroup
literalsUint64ToBigint = subgroup "uint64ToBigint" [
  test "zero" 0 0,
  test "typical value" 1000000 1000000]
  where
    test name x result = primCase name _literals_uint64ToBigint [uint64 x] (bigint result)

-- Bigint to other integer conversions

literalsBigintToInt8 :: TTerm TestGroup
literalsBigintToInt8 = subgroup "bigintToInt8" [
  test "positive" 42 42,
  test "negative" (-42) (-42)]
  where
    test name x result = primCase name _literals_bigintToInt8 [bigint x] (int8 result)

literalsBigintToInt16 :: TTerm TestGroup
literalsBigintToInt16 = subgroup "bigintToInt16" [
  test "positive" 1000 1000,
  test "negative" (-1000) (-1000)]
  where
    test name x result = primCase name _literals_bigintToInt16 [bigint x] (int16 result)

literalsBigintToInt64 :: TTerm TestGroup
literalsBigintToInt64 = subgroup "bigintToInt64" [
  test "positive" 1000000 1000000,
  test "negative" (-1000000) (-1000000)]
  where
    test name x result = primCase name _literals_bigintToInt64 [bigint x] (int64 result)

literalsBigintToUint8 :: TTerm TestGroup
literalsBigintToUint8 = subgroup "bigintToUint8" [
  test "zero" 0 0,
  test "typical value" 100 100]
  where
    test name x result = primCase name _literals_bigintToUint8 [bigint x] (uint8 result)

literalsBigintToUint16 :: TTerm TestGroup
literalsBigintToUint16 = subgroup "bigintToUint16" [
  test "zero" 0 0,
  test "typical value" 1000 1000]
  where
    test name x result = primCase name _literals_bigintToUint16 [bigint x] (uint16 result)

literalsBigintToUint32 :: TTerm TestGroup
literalsBigintToUint32 = subgroup "bigintToUint32" [
  test "zero" 0 0,
  test "typical value" 100000 100000]
  where
    test name x result = primCase name _literals_bigintToUint32 [bigint x] (uint32 result)

literalsBigintToUint64 :: TTerm TestGroup
literalsBigintToUint64 = subgroup "bigintToUint64" [
  test "zero" 0 0,
  test "typical value" 1000000 1000000]
  where
    test name x result = primCase name _literals_bigintToUint64 [bigint x] (uint64 result)

-- Float conversions

literalsFloat32ToBigfloat :: TTerm TestGroup
literalsFloat32ToBigfloat = subgroup "float32ToBigfloat" [
  test "positive" 2.5 2.5,  -- use exact Float32 values
  test "negative" (-2.5) (-2.5),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name _literals_float32ToBigfloat [float32 x] (bigfloat result)

literalsFloat64ToBigfloat :: TTerm TestGroup
literalsFloat64ToBigfloat = subgroup "float64ToBigfloat" [
  test "positive" 3.14159 3.14159,
  test "negative" (-2.71828) (-2.71828),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name _literals_float64ToBigfloat [float64 x] (bigfloat result)

literalsBigfloatToFloat32 :: TTerm TestGroup
literalsBigfloatToFloat32 = subgroup "bigfloatToFloat32" [
  test "positive" 3.14 3.14,
  test "negative" (-2.5) (-2.5),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name _literals_bigfloatToFloat32 [bigfloat x] (float32 result)

literalsBigfloatToFloat64 :: TTerm TestGroup
literalsBigfloatToFloat64 = subgroup "bigfloatToFloat64" [
  test "positive" 3.14159 3.14159,
  test "negative" (-2.71828) (-2.71828),
  test "zero" 0.0 0.0]
  where
    test name x result = primCase name _literals_bigfloatToFloat64 [bigfloat x] (float64 result)

literalsBigintToBigfloat :: TTerm TestGroup
literalsBigintToBigfloat = subgroup "bigintToBigfloat" [
  test "positive" 42 42.0,
  test "negative" (-42) (-42.0),
  test "zero" 0 0.0]
  where
    test name x result = primCase name _literals_bigintToBigfloat [bigint x] (bigfloat result)

literalsBigfloatToBigint :: TTerm TestGroup
literalsBigfloatToBigint = subgroup "bigfloatToBigint" [
  test "positive" 42.7 43,  -- round uses banker's rounding
  test "negative" (-42.7) (-43),
  test "zero" 0.0 0,
  test "round down" 42.3 42,
  test "half even up" 42.5 42,  -- banker's rounding: 42.5 rounds to 42 (even)
  test "half even down" 43.5 44]  -- banker's rounding: 43.5 rounds to 44 (even)
  where
    test name x result = primCase name _literals_bigfloatToBigint [bigfloat x] (bigint result)

-- Show functions

literalsShowInt32 :: TTerm TestGroup
literalsShowInt32 = subgroup "showInt32" [
  test "positive" 42 "42",
  test "negative" (-42) "-42",
  test "zero" 0 "0"]
  where
    test name x result = primCase name _literals_showInt32 [int32 x] (string result)

literalsShowInt8 :: TTerm TestGroup
literalsShowInt8 = subgroup "showInt8" [
  test "positive" 42 "42",
  test "negative" (-42) "-42"]
  where
    test name x result = primCase name _literals_showInt8 [int8 x] (string result)

literalsShowInt16 :: TTerm TestGroup
literalsShowInt16 = subgroup "showInt16" [
  test "positive" 1000 "1000",
  test "negative" (-1000) "-1000"]
  where
    test name x result = primCase name _literals_showInt16 [int16 x] (string result)

literalsShowInt64 :: TTerm TestGroup
literalsShowInt64 = subgroup "showInt64" [
  test "positive" 1000000 "1000000",
  test "negative" (-1000000) "-1000000"]
  where
    test name x result = primCase name _literals_showInt64 [int64 x] (string result)

literalsShowUint8 :: TTerm TestGroup
literalsShowUint8 = subgroup "showUint8" [
  test "zero" 0 "0",
  test "max value" 255 "255"]
  where
    test name x result = primCase name _literals_showUint8 [uint8 x] (string result)

literalsShowUint16 :: TTerm TestGroup
literalsShowUint16 = subgroup "showUint16" [
  test "zero" 0 "0",
  test "typical value" 1000 "1000"]
  where
    test name x result = primCase name _literals_showUint16 [uint16 x] (string result)

literalsShowUint32 :: TTerm TestGroup
literalsShowUint32 = subgroup "showUint32" [
  test "zero" 0 "0",
  test "typical value" 100000 "100000"]
  where
    test name x result = primCase name _literals_showUint32 [uint32 x] (string result)

literalsShowUint64 :: TTerm TestGroup
literalsShowUint64 = subgroup "showUint64" [
  test "zero" 0 "0",
  test "typical value" 1000000 "1000000"]
  where
    test name x result = primCase name _literals_showUint64 [uint64 x] (string result)

literalsShowBigint :: TTerm TestGroup
literalsShowBigint = subgroup "showBigint" [
  test "positive" 42 "42",
  test "negative" (-42) "-42",
  test "zero" 0 "0"]
  where
    test name x result = primCase name _literals_showBigint [bigint x] (string result)

literalsShowFloat32 :: TTerm TestGroup
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

literalsShowFloat64 :: TTerm TestGroup
literalsShowFloat64 = subgroup "showFloat64" [
  test "positive" 3.14159 "3.14159",
  test "zero" 0.0 "0.0",
  test "small positive" 0.05 "5.0e-2",
  test "small positive 2" 0.03 "3.0e-2",
  test "very small" 0.001 "1.0e-3",
  test "normal decimal" 0.1 "0.1"]
  where
    test name x result = primCase name _literals_showFloat64 [float64 x] (string result)

literalsShowBigfloat :: TTerm TestGroup
literalsShowBigfloat = subgroup "showBigfloat" [
  test "positive" 3.14 "3.14",
  test "zero" 0.0 "0.0",
  test "small positive" 0.05 "5.0e-2",
  test "small positive 2" 0.03 "3.0e-2",
  test "very small" 0.001 "1.0e-3",
  test "normal decimal" 0.1 "0.1"]
  where
    test name x result = primCase name _literals_showBigfloat [bigfloat x] (string result)

literalsShowBoolean :: TTerm TestGroup
literalsShowBoolean = subgroup "showBoolean" [
  test "true" true "true",
  test "false" false "false"]
  where
    test name x result = primCase name _literals_showBoolean [x] (string result)

literalsShowString :: TTerm TestGroup
literalsShowString = subgroup "showString" [
  test "simple" "hello" "\"hello\"",
  test "empty" "" "\"\""]
  where
    test name x result = primCase name _literals_showString [string x] (string result)

-- Read functions

literalsReadInt8 :: TTerm TestGroup
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

literalsReadInt16 :: TTerm TestGroup
literalsReadInt16 = subgroup "readInt16" [
  testJust "positive" "1000" 1000,
  testJust "negative" "-1000" (-1000),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readInt16 [string x] (Core.termMaybe $ just (int16 result))
    testNothing name x = primCase name _literals_readInt16 [string x] (Core.termMaybe nothing)

literalsReadInt32 :: TTerm TestGroup
literalsReadInt32 = subgroup "readInt32" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readInt32 [string x] (Core.termMaybe $ just (int32 result))
    testNothing name x = primCase name _literals_readInt32 [string x] (Core.termMaybe nothing)

literalsReadInt64 :: TTerm TestGroup
literalsReadInt64 = subgroup "readInt64" [
  testJust "positive" "1000000" 1000000,
  testJust "negative" "-1000000" (-1000000),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readInt64 [string x] (Core.termMaybe $ just (int64 result))
    testNothing name x = primCase name _literals_readInt64 [string x] (Core.termMaybe nothing)

literalsReadFloat32 :: TTerm TestGroup
literalsReadFloat32 = subgroup "readFloat32" [
  testJust "positive" "3.14" 3.14,
  testJust "negative" "-2.5" (-2.5),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readFloat32 [string x] (Core.termMaybe $ just (float32 result))
    testNothing name x = primCase name _literals_readFloat32 [string x] (Core.termMaybe nothing)

literalsReadFloat64 :: TTerm TestGroup
literalsReadFloat64 = subgroup "readFloat64" [
  testJust "positive" "3.14159" 3.14159,
  testJust "negative" "-2.71828" (-2.71828),
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readFloat64 [string x] (Core.termMaybe $ just (float64 result))
    testNothing name x = primCase name _literals_readFloat64 [string x] (Core.termMaybe nothing)

literalsReadBigfloat :: TTerm TestGroup
literalsReadBigfloat = subgroup "readBigfloat" [
  testJust "positive" "3.14" 3.14,
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readBigfloat [string x] (Core.termMaybe $ just (bigfloat result))
    testNothing name x = primCase name _literals_readBigfloat [string x] (Core.termMaybe nothing)

literalsReadBoolean :: TTerm TestGroup
literalsReadBoolean = subgroup "readBoolean" [
  testJust "true" "true" true,
  testJust "false" "false" false,
  testNothing "invalid" "yes"]
  where
    testJust name x result = primCase name _literals_readBoolean [string x] (Core.termMaybe $ just result)
    testNothing name x = primCase name _literals_readBoolean [string x] (Core.termMaybe nothing)

literalsReadString :: TTerm TestGroup
literalsReadString = subgroup "readString" [
  testJust "quoted string" "\"hello\"" "hello",
  testJust "empty quoted" "\"\"" "",
  testNothing "unquoted" "hello"]
  where
    testJust name x result = primCase name _literals_readString [string x] (Core.termMaybe $ just (string result))
    testNothing name x = primCase name _literals_readString [string x] (Core.termMaybe nothing)

literalsReadUint8 :: TTerm TestGroup
literalsReadUint8 = subgroup "readUint8" [
  testJust "zero" "0" 0,
  testJust "typical" "100" 100,
  testJust "max value" "255" 255,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint8 [string x] (Core.termMaybe $ just (uint8 result))
    testNothing name x = primCase name _literals_readUint8 [string x] (Core.termMaybe nothing)

literalsReadUint16 :: TTerm TestGroup
literalsReadUint16 = subgroup "readUint16" [
  testJust "zero" "0" 0,
  testJust "typical" "1000" 1000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint16 [string x] (Core.termMaybe $ just (uint16 result))
    testNothing name x = primCase name _literals_readUint16 [string x] (Core.termMaybe nothing)

literalsReadUint32 :: TTerm TestGroup
literalsReadUint32 = subgroup "readUint32" [
  testJust "zero" "0" 0,
  testJust "typical" "100000" 100000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint32 [string x] (Core.termMaybe $ just (uint32 result))
    testNothing name x = primCase name _literals_readUint32 [string x] (Core.termMaybe nothing)

literalsReadUint64 :: TTerm TestGroup
literalsReadUint64 = subgroup "readUint64" [
  testJust "zero" "0" 0,
  testJust "typical" "1000000" 1000000,
  testNothing "invalid" "abc",
  testNothing "negative" "-1"]
  where
    testJust name x result = primCase name _literals_readUint64 [string x] (Core.termMaybe $ just (uint64 result))
    testNothing name x = primCase name _literals_readUint64 [string x] (Core.termMaybe nothing)

literalsReadBigint :: TTerm TestGroup
literalsReadBigint = subgroup "readBigint" [
  testJust "positive" "42" 42,
  testJust "negative" "-42" (-42),
  testJust "zero" "0" 0,
  testJust "large" "123456789012345678901234567890" 123456789012345678901234567890,
  testNothing "invalid" "abc"]
  where
    testJust name x result = primCase name _literals_readBigint [string x] (Core.termMaybe $ just (bigint result))
    testNothing name x = primCase name _literals_readBigint [string x] (Core.termMaybe nothing)

-- Binary/String conversion
-- Note: binaryToStringBS and stringToBinary use base64 encoding

literalsStringToBinary :: TTerm TestGroup
literalsStringToBinary = subgroup "stringToBinary" [
  test "simple base64" "aGVsbG8=" (BC.pack "hello"),
  test "empty string" "" B.empty]
  where
    test name x result = primCase name _literals_stringToBinary [string x] (binary result)

literalsBinaryToString :: TTerm TestGroup
literalsBinaryToString = subgroup "binaryToString" [
  test "simple binary" (BC.pack "hello") "aGVsbG8=",
  test "empty binary" B.empty ""]
  where
    test name x result = primCase name _literals_binaryToString [binary x] (string result)

allTests :: TBinding TestGroup
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
      literalsFloat32ToBigfloat,
      literalsFloat64ToBigfloat,
      literalsBigfloatToFloat32,
      literalsBigfloatToFloat64,
      literalsBigintToBigfloat,
      literalsBigfloatToBigint,
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
      literalsShowBigfloat,
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
      literalsReadBigfloat,
      literalsReadBoolean,
      literalsReadString,
      -- Binary conversions
      literalsStringToBinary,
      literalsBinaryToString]
