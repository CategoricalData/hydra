-- | Primitive declarations for the hydra.core.lib.literals namespace.

module Hydra.Sources.Kernel.Lib.Literals where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.core.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.literals module.")}
  where
    definitions = [base64ToBinary,
                   bigintToDecimal, bigintToInt16, bigintToInt32, bigintToInt64, bigintToInt8,
                   bigintToUint16, bigintToUint32, bigintToUint64, bigintToUint8,
                   binaryToBase64, binaryToBytes,
                   decimalToBigint, decimalToFloat32, decimalToFloat64,
                   float32ToDecimal, float32ToFloat64,
                   float64ToDecimal, float64ToFloat32,
                   int16ToBigint, int32ToBigint, int64ToBigint, int8ToBigint,
                   parseBigint, parseBoolean, parseDecimal, parseFloat32, parseFloat64,
                   parseInt16, parseInt32, parseInt64, parseInt8,
                   parseString,
                   parseUint16, parseUint32, parseUint64, parseUint8,
                   printBigint, printBoolean, printDecimal, printFloat32, printFloat64,
                   printInt16, printInt32, printInt64, printInt8,
                   printString,
                   printUint16, printUint32, printUint64, printUint8,
                   uint16ToBigint, uint32ToBigint, uint64ToBigint, uint8ToBigint]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

-- Build a monomorphic signature `a -> b` with an authored (name, description) pair for the single value parameter.
fn :: (String, String) -> Type -> Type -> TermSignature
fn param a b = sigWithParams [param] $ TypeScheme [] (a Types.~> b) mempty

base64ToBinary :: PrimitiveDefinition
base64ToBinary = withProvisions [
  requirement "decodesStandardBase64"
    "base64ToBinary decodes a standard base64 (RFC 4648) ASCII string to the binary data it\
    \ represents, including the empty string decoding to empty binary data."]
  $ define "base64ToBinary" "Decode a base64 ASCII string to binary data." (fn ("s", "the base64 string to decode") Types.string Types.binary)
  ["base64ToBinary(s) decodes the standard base64 (RFC 4648) ASCII string s and returns the resulting\
  \ bytes as binary data.",
   "Inverse of binaryToBase64: base64ToBinary(binaryToBase64(b)) is b for every binary value. Behavior\
  \ on strings which are not valid base64 is host-defined."]

bigintToDecimal :: PrimitiveDefinition
bigintToDecimal = withProvisions [
  requirement "losslessConversion"
    "bigintToDecimal returns the exact decimal representation of a bigint, with no precision loss,\
    \ for positive, negative, and zero values."]
  $ define "bigintToDecimal" "Convert a bigint to a decimal." (fn ("x", "the bigint to convert") Types.bigint Types.decimal)
  ["bigintToDecimal(x) returns the exact decimal representation of x. Since decimal is\
  \ arbitrary-precision and bigint is also arbitrary-precision, the conversion is lossless.",
   "Total."]

bigintToInt16 :: PrimitiveDefinition
bigintToInt16 = withProvisions [
  requirement "convertsInRange"
    "bigintToInt16 returns the int16 with the same numeric value as the bigint, for positive and\
    \ negative values within the int16 range.",
  requirement "wrapsTwosComplementOutOfRange"
    "bigintToInt16 on a value outside the int16 range ([-2^15, 2^15-1]) wraps silently to the\
    \ corresponding two's-complement int16, with no exception, at both the positive and negative\
    \ (all-bits-set) boundary."]
  $ define "bigintToInt16" "Convert a bigint to an int16 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.int16)
  ["bigintToInt16(x) returns x reduced modulo 2^16 and reinterpreted as a signed two's-complement\
  \ int16. Values outside [-2^15, 2^15-1] wrap silently with no exception.",
   "Total."]

bigintToInt32 :: PrimitiveDefinition
bigintToInt32 = withProvisions [
  requirement "convertsInRange"
    "bigintToInt32 returns the int32 with the same numeric value as the bigint, for positive and\
    \ negative values within the int32 range.",
  requirement "wrapsTwosComplementOutOfRange"
    "bigintToInt32 on a value outside the int32 range ([-2^31, 2^31-1]) wraps silently to the\
    \ corresponding two's-complement int32, with no exception, at both the positive and negative\
    \ (all-bits-set) boundary; this holds even on hosts where int32 is represented as a wider native\
    \ integer, which must not skip the narrowing step."]
  $ define "bigintToInt32" "Convert a bigint to an int32 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.int32)
  ["bigintToInt32(x) returns x reduced modulo 2^32 and reinterpreted as a signed two's-complement\
  \ int32. Values outside [-2^31, 2^31-1] wrap silently with no exception.",
   "Total."]

bigintToInt64 :: PrimitiveDefinition
bigintToInt64 = withProvisions [
  requirement "convertsInRange"
    "bigintToInt64 returns the int64 with the same numeric value as the bigint, for positive and\
    \ negative values within the int64 range."]
  $ define "bigintToInt64" "Convert a bigint to an int64 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.int64)
  ["bigintToInt64(x) returns x reduced modulo 2^64 and reinterpreted as a signed two's-complement\
  \ int64. Values outside [-2^63, 2^63-1] wrap silently with no exception.",
   "Total."]

bigintToInt8 :: PrimitiveDefinition
bigintToInt8 = withProvisions [
  requirement "convertsInRange"
    "bigintToInt8 returns the int8 with the same numeric value as the bigint, for positive and\
    \ negative values within the int8 range.",
  requirement "wrapsTwosComplementOutOfRange"
    "bigintToInt8 on a value outside the int8 range ([-128, 127]) wraps silently to the\
    \ corresponding two's-complement int8, with no exception, at both the positive and negative\
    \ (all-bits-set) boundary."]
  $ define "bigintToInt8" "Convert a bigint to an int8 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.int8)
  ["bigintToInt8(x) returns x reduced modulo 2^8 and reinterpreted as a signed two's-complement\
  \ int8. Values outside [-128, 127] wrap silently with no exception.",
   "Total."]

bigintToUint16 :: PrimitiveDefinition
bigintToUint16 = withProvisions [
  requirement "convertsInRange"
    "bigintToUint16 returns the uint16 with the same numeric value as the bigint, for a zero or\
    \ typical in-range value."]
  $ define "bigintToUint16" "Convert a bigint to a uint16 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.uint16)
  ["bigintToUint16(x) returns x reduced modulo 2^16 as an unsigned uint16. Values outside [0, 2^16-1]\
  \ wrap silently with no exception.",
   "Total."]

bigintToUint32 :: PrimitiveDefinition
bigintToUint32 = withProvisions [
  requirement "convertsInRange"
    "bigintToUint32 returns the uint32 with the same numeric value as the bigint, for a zero or\
    \ typical in-range value."]
  $ define "bigintToUint32" "Convert a bigint to a uint32 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.uint32)
  ["bigintToUint32(x) returns x reduced modulo 2^32 as an unsigned uint32. Values outside [0, 2^32-1]\
  \ wrap silently with no exception.",
   "Total."]

bigintToUint64 :: PrimitiveDefinition
bigintToUint64 = withProvisions [
  requirement "convertsInRange"
    "bigintToUint64 returns the uint64 with the same numeric value as the bigint, for a zero or\
    \ typical in-range value."]
  $ define "bigintToUint64" "Convert a bigint to a uint64 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.uint64)
  ["bigintToUint64(x) returns x reduced modulo 2^64 as an unsigned uint64. Values outside [0, 2^64-1]\
  \ wrap silently with no exception.",
   "Total."]

bigintToUint8 :: PrimitiveDefinition
bigintToUint8 = withProvisions [
  requirement "convertsInRange"
    "bigintToUint8 returns the uint8 with the same numeric value as the bigint, for a zero or\
    \ typical in-range value."]
  $ define "bigintToUint8" "Convert a bigint to a uint8 (truncating)." (fn ("x", "the bigint to convert") Types.bigint Types.uint8)
  ["bigintToUint8(x) returns x reduced modulo 2^8 as an unsigned uint8. Values outside [0, 255] wrap\
  \ silently with no exception.",
   "Total."]

binaryToBase64 :: PrimitiveDefinition
binaryToBase64 = withProvisions [
  requirement "encodesStandardBase64"
    "binaryToBase64 encodes binary data as a standard base64 (RFC 4648) ASCII string, including\
    \ empty binary data encoding to the empty string."]
  $ define "binaryToBase64" "Encode binary data as a base64 ASCII string." (fn ("b", "the binary data to encode") Types.binary Types.string)
  ["binaryToBase64(b) encodes the bytes of b using standard base64 (RFC 4648) and returns the result\
  \ as an ASCII string.",
   "Total and round-trippable: base64ToBinary(binaryToBase64(b)) is b for every binary value. Use this\
  \ to embed arbitrary binary data in a string context (e.g. a JSON value). For interpreting bytes as\
  \ text, use hydra.core.lib.text.decodeUtf8 instead."]

binaryToBytes :: PrimitiveDefinition
binaryToBytes = withProvisions [
  requirement "returnsUnsignedByteValues"
    "binaryToBytes returns the bytes of the binary data as a list of int32 values in [0, 255],\
    \ preserving byte order, including byte values above 127 (which remain unsigned, not\
    \ reinterpreted as negative), and the empty list for empty binary data."]
  $ define "binaryToBytes" "Convert binary data to a list of byte values."
  (fn ("b", "the binary data to convert") Types.binary (Types.list Types.int32))
  ["binaryToBytes(b) returns the bytes of b as an int32 list with values in [0, 255], preserving byte\
  \ order.",
   "The result list element type is int32 rather than uint8 because Hydra's primitive collections work\
  \ most naturally with int32 indices.",
   "Total."]

decimalToBigint :: PrimitiveDefinition
decimalToBigint = withProvisions [
  requirement "convertsWholeNumbers"
    "decimalToBigint returns the bigint with the same numeric value as a whole-number decimal, for\
    \ positive, negative, and zero values.",
  requirement "truncatesFractionTowardZero"
    "decimalToBigint on a decimal with a fractional part discards the fraction, truncating toward\
    \ zero (rounding down for positive values, up for negative values), not rounding to nearest."]
  $ define "decimalToBigint" "Convert a decimal to a bigint (truncating)." (fn ("x", "the decimal to convert") Types.decimal Types.bigint)
  ["decimalToBigint(x) returns x truncated toward zero. Fractional parts are discarded.",
   "Total."]

decimalToFloat32 :: PrimitiveDefinition
decimalToFloat32 = withProvisions [
  requirement "convertsExactlyRepresentableValues"
    "decimalToFloat32 returns the float32 value with the same numeric value as a decimal that is\
    \ exactly representable in binary32, for zero and whole-number values."]
  $ define "decimalToFloat32" "Convert a decimal to a float32." (fn ("x", "the decimal to convert") Types.decimal Types.float32)
  ["decimalToFloat32(x) returns the IEEE 754 binary32 value closest to x under roundTiesToEven. Values\
  \ outside the float32 representable range overflow to \xB1\x221E. Subnormal precision loss is silent.",
   "Total."]

decimalToFloat64 :: PrimitiveDefinition
decimalToFloat64 = withProvisions [
  requirement "convertsExactlyRepresentableValues"
    "decimalToFloat64 returns the float64 value with the same numeric value as a decimal that is\
    \ exactly representable in binary64, for zero and whole-number values."]
  $ define "decimalToFloat64" "Convert a decimal to a float64." (fn ("x", "the decimal to convert") Types.decimal Types.float64)
  ["decimalToFloat64(x) returns the IEEE 754 binary64 value closest to x under roundTiesToEven. Values\
  \ outside the float64 representable range overflow to \xB1\x221E. Subnormal precision loss is silent.",
   "Total."]

float32ToDecimal :: PrimitiveDefinition
float32ToDecimal = withProvisions [
  requirement "convertsExactlyRepresentableValues"
    "float32ToDecimal returns the exact decimal representation of a binary32 value, for zero and\
    \ whole-number values."]
  $ define "float32ToDecimal" "Convert a float32 to a decimal." (fn ("x", "the float32 to convert") Types.float32 Types.decimal)
  ["float32ToDecimal(x) returns the exact decimal representation of the binary32 value x. Special\
  \ float32 values (\xB1\x221E, NaN) have a host-defined decimal representation; finite values convert\
  \ exactly since binary32 values are dyadic rationals.",
   "Total."]

float32ToFloat64 :: PrimitiveDefinition
float32ToFloat64 = withProvisions [
  requirement "losslessWidening"
    "float32ToFloat64 returns the float64 value with the same numeric value as a float32, with no\
    \ precision loss, for positive, negative, and zero values."]
  $ define "float32ToFloat64" "Convert a float32 to a float64." (fn ("x", "the float32 to convert") Types.float32 Types.float64)
  ["float32ToFloat64(x) returns the binary64 value with the same numeric value as x; \xB1\x221E and NaN\
  \ pass through. The conversion is exact and lossless.",
   "Total."]

float64ToDecimal :: PrimitiveDefinition
float64ToDecimal = withProvisions [
  requirement "convertsExactlyRepresentableValues"
    "float64ToDecimal returns the exact decimal representation of a binary64 value, for zero and\
    \ whole-number values."]
  $ define "float64ToDecimal" "Convert a float64 to a decimal." (fn ("x", "the float64 to convert") Types.float64 Types.decimal)
  ["float64ToDecimal(x) returns the exact decimal representation of the binary64 value x. Special\
  \ float64 values (\xB1\x221E, NaN) have a host-defined decimal representation; finite values convert\
  \ exactly since binary64 values are dyadic rationals.",
   "Total."]

float64ToFloat32 :: PrimitiveDefinition
float64ToFloat32 = withProvisions [
  requirement "convertsExactlyRepresentableValues"
    "float64ToFloat32 returns the float32 value with the same numeric value as a float64 that is\
    \ exactly representable in binary32, for positive, negative, and zero values."]
  $ define "float64ToFloat32" "Convert a float64 to a float32 (lossy)." (fn ("x", "the float64 to convert") Types.float64 Types.float32)
  ["float64ToFloat32(x) returns the binary32 value closest to x under roundTiesToEven. Values outside\
  \ the binary32 range overflow to \xB1\x221E; subnormal precision loss is silent; NaN payload may be\
  \ canonicalized by the host.",
   "Total."]

int16ToBigint :: PrimitiveDefinition
int16ToBigint = withProvisions [
  requirement "losslessConversion"
    "int16ToBigint returns the bigint with the same numeric value as the int16, with no precision\
    \ loss, for positive and negative values."]
  $ define "int16ToBigint" "Convert an int16 to a bigint." (fn ("x", "the int16 to convert") Types.int16 Types.bigint)
  ["int16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

int32ToBigint :: PrimitiveDefinition
int32ToBigint = withProvisions [
  requirement "losslessConversion"
    "int32ToBigint returns the bigint with the same numeric value as the int32, with no precision\
    \ loss, for positive, negative, and zero values."]
  $ define "int32ToBigint" "Convert an int32 to a bigint." (fn ("x", "the int32 to convert") Types.int32 Types.bigint)
  ["int32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

int64ToBigint :: PrimitiveDefinition
int64ToBigint = withProvisions [
  requirement "losslessConversion"
    "int64ToBigint returns the bigint with the same numeric value as the int64, with no precision\
    \ loss, for positive and negative values."]
  $ define "int64ToBigint" "Convert an int64 to a bigint." (fn ("x", "the int64 to convert") Types.int64 Types.bigint)
  ["int64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

int8ToBigint :: PrimitiveDefinition
int8ToBigint = withProvisions [
  requirement "losslessConversion"
    "int8ToBigint returns the bigint with the same numeric value as the int8, with no precision\
    \ loss, for positive and negative values including the int8 min/max boundary."]
  $ define "int8ToBigint" "Convert an int8 to a bigint." (fn ("x", "the int8 to convert") Types.int8 Types.bigint)
  ["int8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

parseBigint :: PrimitiveDefinition
parseBigint = withProvisions [
  requirement "parsesValidLiterals"
    "parseBigint returns the bigint parsed from a valid bigint literal, wrapped, for positive,\
    \ negative, zero, and arbitrarily large values.",
  requirement "noneOnInvalidInput"
    "parseBigint returns none when the string is not a valid bigint literal."]
  $ define "parseBigint" "Parse a string as a bigint."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.bigint))
  ["parseBigint(s) returns Just(x) where x is the bigint parsed from s, or Nothing if s is not a valid\
  \ bigint literal.",
   "Accepted syntax follows the Haskell Show/Read convention: an optional leading minus sign followed\
  \ by a sequence of decimal digits. Whitespace is not stripped; leading or trailing whitespace causes\
  \ parse failure.",
   "Total."]

parseBoolean :: PrimitiveDefinition
parseBoolean = withProvisions [
  requirement "parsesExactLowercaseLiterals"
    "parseBoolean returns the wrapped boolean true for the exact string \"true\" and false for\
    \ \"false\".",
  requirement "noneOnInvalidInput"
    "parseBoolean returns none for any string other than the exact lowercase literals \"true\" or\
    \ \"false\"."]
  $ define "parseBoolean" "Parse a string as a boolean."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.boolean))
  ["parseBoolean(s) returns Just(true) for \"true\", Just(false) for \"false\", and Nothing for any\
  \ other input (including \"True\"/\"False\" with capital initial letter on some hosts; behavior on\
  \ capitalized forms is host-defined and should not be relied upon).",
   "Total."]

parseDecimal :: PrimitiveDefinition
parseDecimal = withProvisions [
  requirement "parsesValidLiterals"
    "parseDecimal returns the decimal parsed from a valid decimal literal, wrapped, for positive,\
    \ negative, and zero values.",
  requirement "noneOnInvalidInput"
    "parseDecimal returns none when the string is not a valid decimal literal."]
  $ define "parseDecimal" "Parse a string as a decimal."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.decimal))
  ["parseDecimal(s) returns Just(x) where x is the decimal parsed from s, or Nothing on parse failure.",
   "Accepted syntax is the standard decimal literal: an optional sign, an integer part, an optional\
  \ fractional part, and an optional exponent (e.g. -1.5, 2e10, 0.0).",
   "Total."]

parseFloat32 :: PrimitiveDefinition
parseFloat32 = withProvisions [
  requirement "parsesValidLiterals"
    "parseFloat32 returns the float32 value closest to the number parsed from a valid literal,\
    \ wrapped, for positive and negative values.",
  requirement "noneOnInvalidInput"
    "parseFloat32 returns none when the string is not a valid float literal."]
  $ define "parseFloat32" "Parse a string as a float32."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.float32))
  ["parseFloat32(s) returns Just(x) where x is the IEEE 754 binary32 value closest to the number parsed\
  \ from s under roundTiesToEven, or Nothing on parse failure.",
   "Accepted special-value literals (\"NaN\", \"Infinity\", \"-Infinity\", etc.) and their\
  \ capitalization are host-defined.",
   "Total."]

parseFloat64 :: PrimitiveDefinition
parseFloat64 = withProvisions [
  requirement "parsesValidLiterals"
    "parseFloat64 returns the float64 value closest to the number parsed from a valid literal,\
    \ wrapped, for positive and negative values.",
  requirement "noneOnInvalidInput"
    "parseFloat64 returns none when the string is not a valid float literal."]
  $ define "parseFloat64" "Parse a string as a float64."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.float64))
  ["parseFloat64(s) returns Just(x) where x is the IEEE 754 binary64 value closest to the number parsed\
  \ from s under roundTiesToEven, or Nothing on parse failure.",
   "Accepted special-value literals (\"NaN\", \"Infinity\", \"-Infinity\", etc.) and their\
  \ capitalization are host-defined.",
   "Total."]

parseInt16 :: PrimitiveDefinition
parseInt16 = withProvisions [
  requirement "parsesValidLiterals"
    "parseInt16 returns the int16 parsed from a valid integer literal within range, wrapped, for\
    \ positive and negative values.",
  requirement "noneOnInvalidInput"
    "parseInt16 returns none when the string is not a valid integer literal."]
  $ define "parseInt16" "Parse a string as an int16."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.int16))
  ["parseInt16(s) returns Just(x) where x is the int16 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-2^15, 2^15-1].",
   "Total."]

parseInt32 :: PrimitiveDefinition
parseInt32 = withProvisions [
  requirement "parsesValidLiterals"
    "parseInt32 returns the int32 parsed from a valid integer literal within range, wrapped, for\
    \ positive and negative values.",
  requirement "noneOnInvalidInput"
    "parseInt32 returns none when the string is not a valid integer literal."]
  $ define "parseInt32" "Parse a string as an int32."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.int32))
  ["parseInt32(s) returns Just(x) where x is the int32 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-2^31, 2^31-1].",
   "Total."]

parseInt64 :: PrimitiveDefinition
parseInt64 = withProvisions [
  requirement "parsesValidLiterals"
    "parseInt64 returns the int64 parsed from a valid integer literal within range, wrapped, for\
    \ positive and negative values.",
  requirement "noneOnInvalidInput"
    "parseInt64 returns none when the string is not a valid integer literal."]
  $ define "parseInt64" "Parse a string as an int64."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.int64))
  ["parseInt64(s) returns Just(x) where x is the int64 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-2^63, 2^63-1].",
   "Total."]

parseInt8 :: PrimitiveDefinition
parseInt8 = withProvisions [
  requirement "parsesValidLiterals"
    "parseInt8 returns the int8 parsed from a valid integer literal within range, wrapped, for\
    \ positive and negative values including the int8 min/max boundary.",
  requirement "noneOnOutOfRangeOrInvalidInput"
    "parseInt8 returns none when the string is not a valid integer literal, or when the parsed\
    \ value is outside the int8 range [-128, 127]."]
  $ define "parseInt8" "Parse a string as an int8."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.int8))
  ["parseInt8(s) returns Just(x) where x is the int8 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-128, 127].",
   "Total."]

parseString :: PrimitiveDefinition
parseString = withProvisions [
  requirement "parsesQuotedLiterals"
    "parseString returns the decoded payload of a double-quoted string literal, wrapped.",
  requirement "noneOnUnquotedInput"
    "parseString returns none when the string is not a well-formed double-quoted string literal\
    \ (for example, an unquoted bare string)."]
  $ define "parseString"
  "Parse a string-literal token to a plain string (Just) or Nothing on parse failure."
  (fn ("s", "the string-literal token to parse") Types.string (Types.optional Types.string))
  ["parseString(s) parses s as a Haskell-syntax string literal token: an opening double-quote, escaped\
  \ characters per the Haskell lexical grammar (backslash escapes for special characters and Unicode\
  \ code points), and a closing double-quote. Returns Just of the decoded payload on success or\
  \ Nothing on malformed input.",
   "Total. The inverse of printString."]

parseUint16 :: PrimitiveDefinition
parseUint16 = withProvisions [
  requirement "parsesValidLiterals"
    "parseUint16 returns the uint16 parsed from a valid non-negative integer literal within range,\
    \ wrapped.",
  requirement "noneOnInvalidOrNegativeInput"
    "parseUint16 returns none when the string is not a valid integer literal, or when it represents\
    \ a negative value."]
  $ define "parseUint16" "Parse a string as a uint16."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.uint16))
  ["parseUint16(s) returns Just(x) where x is the uint16 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 2^16-1].",
   "Total."]

parseUint32 :: PrimitiveDefinition
parseUint32 = withProvisions [
  requirement "parsesValidLiterals"
    "parseUint32 returns the uint32 parsed from a valid non-negative integer literal within range,\
    \ wrapped, including at the maximum representable value (2^32-1).",
  requirement "noneOnInvalidOrNegativeInput"
    "parseUint32 returns none when the string is not a valid integer literal, or when it represents\
    \ a negative value.",
  requirement "noneOnOverflow"
    "parseUint32 returns none when the parsed value exceeds the uint32 maximum (2^32-1), one past\
    \ the boundary."]
  $ define "parseUint32" "Parse a string as a uint32."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.uint32))
  ["parseUint32(s) returns Just(x) where x is the uint32 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 2^32-1].",
   "Total."]

parseUint64 :: PrimitiveDefinition
parseUint64 = withProvisions [
  requirement "parsesValidLiterals"
    "parseUint64 returns the uint64 parsed from a valid non-negative integer literal within range,\
    \ wrapped, including large in-range values.",
  requirement "noneOnInvalidOrNegativeInput"
    "parseUint64 returns none when the string is not a valid integer literal, or when it represents\
    \ a negative value.",
  requirement "noneOnOverflow"
    "parseUint64 returns none when the parsed value exceeds the uint64 maximum (2^64-1), one past\
    \ the boundary."]
  $ define "parseUint64" "Parse a string as a uint64."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.uint64))
  ["parseUint64(s) returns Just(x) where x is the uint64 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 2^64-1].",
   "Total."]

parseUint8 :: PrimitiveDefinition
parseUint8 = withProvisions [
  requirement "parsesValidLiterals"
    "parseUint8 returns the uint8 parsed from a valid non-negative integer literal within range,\
    \ wrapped, including at the maximum representable value (255).",
  requirement "noneOnInvalidOrNegativeInput"
    "parseUint8 returns none when the string is not a valid integer literal, or when it represents\
    \ a negative value."]
  $ define "parseUint8" "Parse a string as a uint8."
  (fn ("s", "the string to parse") Types.string (Types.optional Types.uint8))
  ["parseUint8(s) returns Just(x) where x is the uint8 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 255].",
   "Total."]

printBigint :: PrimitiveDefinition
printBigint = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printBigint renders a bigint as its canonical decimal string, with a leading minus sign for\
    \ negative values and no leading sign for positive values or zero."]
  $ define "printBigint" "Render a bigint as a string." (fn ("x", "the bigint to render") Types.bigint Types.string)
  ["printBigint(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits. No leading zeros are produced (except for the value 0 itself, which\
  \ renders as \"0\").",
   "Total. The inverse of parseBigint."]

printBoolean :: PrimitiveDefinition
printBoolean = withProvisions [
  requirement "rendersLowercaseLiterals"
    "printBoolean renders true as the exact string \"true\" and false as \"false\"."]
  $ define "printBoolean" "Render a boolean as a string." (fn ("b", "the boolean to render") Types.boolean Types.string)
  ["printBoolean(true) = \"true\"; printBoolean(false) = \"false\".",
   "Total. The inverse of parseBoolean."]

printDecimal :: PrimitiveDefinition
printDecimal = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printDecimal renders a decimal as a string, for positive and negative whole and fractional\
    \ values, that always round-trips through parseDecimal back to the original value."]
  $ define "printDecimal" "Render a decimal as a string." (fn ("x", "the decimal to render") Types.decimal Types.string)
  ["printDecimal(x) returns the canonical decimal representation of x as a string. The exact form (use\
  \ of scientific notation, trailing zeros, etc.) is host-defined but always round-trips with\
  \ parseDecimal.",
   "Total."]

printFloat32 :: PrimitiveDefinition
printFloat32 = withProvisions [
  requirement "rendersRoundTrippableRepresentation"
    "printFloat32 renders a float32 as a string, for positive, negative, zero, and small-magnitude\
    \ values, that round-trips through parseFloat32 back to the original value."]
  $ define "printFloat32" "Render a float32 as a string." (fn ("x", "the float32 to render") Types.float32 Types.string)
  ["printFloat32(x) returns a string representation of x. Finite values use the shortest decimal that\
  \ round-trips through parseFloat32 back to x. Special values render as \"NaN\", \"Infinity\", or\
  \ \"-Infinity\" (capitalization is host-defined).",
   "Total."]

printFloat64 :: PrimitiveDefinition
printFloat64 = withProvisions [
  requirement "rendersRoundTrippableRepresentation"
    "printFloat64 renders a float64 as a string, for positive, zero, and small-magnitude values,\
    \ that round-trips through parseFloat64 back to the original value."]
  $ define "printFloat64" "Render a float64 as a string." (fn ("x", "the float64 to render") Types.float64 Types.string)
  ["printFloat64(x) returns a string representation of x. Finite values use the shortest decimal that\
  \ round-trips through parseFloat64 back to x. Special values render as \"NaN\", \"Infinity\", or\
  \ \"-Infinity\" (capitalization is host-defined).",
   "Total."]

printInt16 :: PrimitiveDefinition
printInt16 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printInt16 renders an int16 as its canonical decimal string, with a leading minus sign for\
    \ negative values."]
  $ define "printInt16" "Render an int16 as a string." (fn ("x", "the int16 to render") Types.int16 Types.string)
  ["printInt16(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of parseInt16."]

printInt32 :: PrimitiveDefinition
printInt32 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printInt32 renders an int32 as its canonical decimal string, with a leading minus sign for\
    \ negative values and no sign for zero."]
  $ define "printInt32" "Render an int32 as a string." (fn ("x", "the int32 to render") Types.int32 Types.string)
  ["printInt32(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of parseInt32."]

printInt64 :: PrimitiveDefinition
printInt64 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printInt64 renders an int64 as its canonical decimal string, with a leading minus sign for\
    \ negative values."]
  $ define "printInt64" "Render an int64 as a string." (fn ("x", "the int64 to render") Types.int64 Types.string)
  ["printInt64(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of parseInt64."]

printInt8 :: PrimitiveDefinition
printInt8 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printInt8 renders an int8 as its canonical decimal string, with a leading minus sign for\
    \ negative values."]
  $ define "printInt8" "Render an int8 as a string." (fn ("x", "the int8 to render") Types.int8 Types.string)
  ["printInt8(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of parseInt8."]

printString :: PrimitiveDefinition
printString = withProvisions [
  requirement "quotesAndPassesThroughPlainCharacters"
    "printString wraps a string in double quotes, passing plain ASCII characters through\
    \ unescaped, for the empty string and ordinary text.",
  requirement "escapesNonAsciiAsDecimalCodes"
    "printString renders every non-ASCII character as a backslash followed by its decimal Unicode\
    \ code point, whether the character appears alone, mixed with ASCII text, or repeated.",
  requirement "escapesNamedControlCharacters"
    "printString renders the characters with standard Haskell-syntax named escapes (tab, newline,\
    \ carriage return, backslash, double quote, NUL, bell, backspace, form feed, vertical tab,\
    \ delete) using their named escape form, not a raw or decimal-code representation."]
  $ define "printString" "Render a string as a string-literal token (escaped and quoted)."
  (fn ("s", "the string to render") Types.string Types.string)
  ["printString(s) returns a Haskell-syntax string literal token representing s: an opening\
  \ double-quote, the characters of s with special characters escaped (backslash sequences for\
  \ control characters, quotes, and non-printable Unicode code points), and a closing double-quote.",
   "Total. The inverse of parseString."]

printUint16 :: PrimitiveDefinition
printUint16 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printUint16 renders a uint16 as its canonical unsigned decimal string, for zero and typical\
    \ values."]
  $ define "printUint16" "Render a uint16 as a string." (fn ("x", "the uint16 to render") Types.uint16 Types.string)
  ["printUint16(x) returns the canonical decimal representation of x.",
   "Total. The inverse of parseUint16."]

printUint32 :: PrimitiveDefinition
printUint32 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printUint32 renders a uint32 as its canonical unsigned decimal string, for zero and typical\
    \ values."]
  $ define "printUint32" "Render a uint32 as a string." (fn ("x", "the uint32 to render") Types.uint32 Types.string)
  ["printUint32(x) returns the canonical decimal representation of x.",
   "Total. The inverse of parseUint32."]

printUint64 :: PrimitiveDefinition
printUint64 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printUint64 renders a uint64 as its canonical unsigned decimal string, for zero and typical\
    \ values."]
  $ define "printUint64" "Render a uint64 as a string." (fn ("x", "the uint64 to render") Types.uint64 Types.string)
  ["printUint64(x) returns the canonical decimal representation of x.",
   "Total. The inverse of parseUint64."]

printUint8 :: PrimitiveDefinition
printUint8 = withProvisions [
  requirement "rendersCanonicalDecimal"
    "printUint8 renders a uint8 as its canonical unsigned decimal string, for zero and the maximum\
    \ representable value (255)."]
  $ define "printUint8" "Render a uint8 as a string." (fn ("x", "the uint8 to render") Types.uint8 Types.string)
  ["printUint8(x) returns the canonical decimal representation of x.",
   "Total. The inverse of parseUint8."]

uint16ToBigint :: PrimitiveDefinition
uint16ToBigint = withProvisions [
  requirement "losslessConversion"
    "uint16ToBigint returns the bigint with the same numeric value as the uint16, with no\
    \ precision loss, for zero and typical in-range values."]
  $ define "uint16ToBigint" "Convert a uint16 to a bigint." (fn ("x", "the uint16 to convert") Types.uint16 Types.bigint)
  ["uint16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

uint32ToBigint :: PrimitiveDefinition
uint32ToBigint = withProvisions [
  requirement "losslessConversion"
    "uint32ToBigint returns the bigint with the same numeric value as the uint32, with no\
    \ precision loss, for zero and typical in-range values."]
  $ define "uint32ToBigint" "Convert a uint32 to a bigint." (fn ("x", "the uint32 to convert") Types.uint32 Types.bigint)
  ["uint32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

uint64ToBigint :: PrimitiveDefinition
uint64ToBigint = withProvisions [
  requirement "losslessConversion"
    "uint64ToBigint returns the bigint with the same numeric value as the uint64, with no\
    \ precision loss, for zero and typical in-range values."]
  $ define "uint64ToBigint" "Convert a uint64 to a bigint." (fn ("x", "the uint64 to convert") Types.uint64 Types.bigint)
  ["uint64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

uint8ToBigint :: PrimitiveDefinition
uint8ToBigint = withProvisions [
  requirement "losslessConversion"
    "uint8ToBigint returns the bigint with the same numeric value as the uint8, with no precision\
    \ loss, for zero and the maximum representable value (255)."]
  $ define "uint8ToBigint" "Convert a uint8 to a bigint." (fn ("x", "the uint8 to convert") Types.uint8 Types.bigint)
  ["uint8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]
