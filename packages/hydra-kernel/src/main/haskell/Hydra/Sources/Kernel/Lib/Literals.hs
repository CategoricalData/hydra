-- | Primitive declarations for the hydra.lib.literals namespace.

module Hydra.Sources.Kernel.Lib.Literals where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), showString)


ns :: ModuleName
ns = ModuleName "hydra.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.literals module.")}
  where
    definitions = [bigintToDecimal, bigintToInt16, bigintToInt32, bigintToInt64, bigintToInt8,
                   bigintToUint16, bigintToUint32, bigintToUint64, bigintToUint8,
                   binaryToBytes, binaryToString,
                   decimalToBigint, decimalToFloat32, decimalToFloat64,
                   float32ToDecimal, float32ToFloat64,
                   float64ToDecimal, float64ToFloat32,
                   int16ToBigint, int32ToBigint, int64ToBigint, int8ToBigint,
                   readBigint, readBoolean, readDecimal, readFloat32, readFloat64,
                   readInt16, readInt32, readInt64, readInt8, readString,
                   readUint16, readUint32, readUint64, readUint8,
                   showBigint, showBoolean, showDecimal, showFloat32, showFloat64,
                   showInt16, showInt32, showInt64, showInt8, showString,
                   showUint16, showUint32, showUint64, showUint8,
                   stringToBinary,
                   uint16ToBigint, uint32ToBigint, uint64ToBigint, uint8ToBigint]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

-- Build a monomorphic signature `a -> b`.
fn :: Type -> Type -> TermSignature
fn a b = sig $ TypeScheme [] (a Types.~> b) Nothing

bigintToDecimal :: PrimitiveDefinition
bigintToDecimal = define "bigintToDecimal" "Convert a bigint to a decimal." (fn Types.bigint Types.decimal)
  ["bigintToDecimal(x) returns the exact decimal representation of x. Since decimal is\
  \ arbitrary-precision and bigint is also arbitrary-precision, the conversion is lossless.",
   "Total."]

bigintToInt16 :: PrimitiveDefinition
bigintToInt16 = define "bigintToInt16" "Convert a bigint to an int16 (truncating)." (fn Types.bigint Types.int16)
  ["bigintToInt16(x) returns x reduced modulo 2^16 and reinterpreted as a signed two's-complement\
  \ int16. Values outside [-2^15, 2^15-1] wrap silently with no exception.",
   "Total."]

bigintToInt32 :: PrimitiveDefinition
bigintToInt32 = define "bigintToInt32" "Convert a bigint to an int32 (truncating)." (fn Types.bigint Types.int32)
  ["bigintToInt32(x) returns x reduced modulo 2^32 and reinterpreted as a signed two's-complement\
  \ int32. Values outside [-2^31, 2^31-1] wrap silently with no exception.",
   "Total."]

bigintToInt64 :: PrimitiveDefinition
bigintToInt64 = define "bigintToInt64" "Convert a bigint to an int64 (truncating)." (fn Types.bigint Types.int64)
  ["bigintToInt64(x) returns x reduced modulo 2^64 and reinterpreted as a signed two's-complement\
  \ int64. Values outside [-2^63, 2^63-1] wrap silently with no exception.",
   "Total."]

bigintToInt8 :: PrimitiveDefinition
bigintToInt8 = define "bigintToInt8" "Convert a bigint to an int8 (truncating)." (fn Types.bigint Types.int8)
  ["bigintToInt8(x) returns x reduced modulo 2^8 and reinterpreted as a signed two's-complement\
  \ int8. Values outside [-128, 127] wrap silently with no exception.",
   "Total."]

bigintToUint16 :: PrimitiveDefinition
bigintToUint16 = define "bigintToUint16" "Convert a bigint to a uint16 (truncating)." (fn Types.bigint Types.uint16)
  ["bigintToUint16(x) returns x reduced modulo 2^16 as an unsigned uint16. Values outside [0, 2^16-1]\
  \ wrap silently with no exception.",
   "Total."]

bigintToUint32 :: PrimitiveDefinition
bigintToUint32 = define "bigintToUint32" "Convert a bigint to a uint32 (truncating)." (fn Types.bigint Types.uint32)
  ["bigintToUint32(x) returns x reduced modulo 2^32 as an unsigned uint32. Values outside [0, 2^32-1]\
  \ wrap silently with no exception.",
   "Total."]

bigintToUint64 :: PrimitiveDefinition
bigintToUint64 = define "bigintToUint64" "Convert a bigint to a uint64 (truncating)." (fn Types.bigint Types.uint64)
  ["bigintToUint64(x) returns x reduced modulo 2^64 as an unsigned uint64. Values outside [0, 2^64-1]\
  \ wrap silently with no exception.",
   "Total."]

bigintToUint8 :: PrimitiveDefinition
bigintToUint8 = define "bigintToUint8" "Convert a bigint to a uint8 (truncating)." (fn Types.bigint Types.uint8)
  ["bigintToUint8(x) returns x reduced modulo 2^8 as an unsigned uint8. Values outside [0, 255] wrap\
  \ silently with no exception.",
   "Total."]

binaryToBytes :: PrimitiveDefinition
binaryToBytes = define "binaryToBytes" "Convert binary data to a list of byte values."
  (fn Types.binary (Types.list Types.int32))
  ["binaryToBytes(b) returns the bytes of b as an int32 list with values in [0, 255], preserving byte\
  \ order.",
   "The result list element type is int32 rather than uint8 because Hydra's primitive collections work\
  \ most naturally with int32 indices.",
   "Total."]

binaryToString :: PrimitiveDefinition
binaryToString = define "binaryToString" "Convert binary data to a UTF-8 string." (fn Types.binary Types.string)
  ["binaryToString(b) interprets the bytes of b as a UTF-8-encoded string and returns the decoded\
  \ value.",
   "The behavior on invalid UTF-8 byte sequences is host-defined: most hosts substitute the replacement\
  \ character U+FFFD; some may signal an error or truncate.",
   "Total in the sense that it does not raise from the kernel's perspective, but the result may carry\
  \ the host's replacement semantics."]

decimalToBigint :: PrimitiveDefinition
decimalToBigint = define "decimalToBigint" "Convert a decimal to a bigint (truncating)." (fn Types.decimal Types.bigint)
  ["decimalToBigint(x) returns x truncated toward zero. Fractional parts are discarded.",
   "Total."]

decimalToFloat32 :: PrimitiveDefinition
decimalToFloat32 = define "decimalToFloat32" "Convert a decimal to a float32." (fn Types.decimal Types.float32)
  ["decimalToFloat32(x) returns the IEEE 754 binary32 value closest to x under roundTiesToEven. Values\
  \ outside the float32 representable range overflow to \xB1\x221E. Subnormal precision loss is silent.",
   "Total."]

decimalToFloat64 :: PrimitiveDefinition
decimalToFloat64 = define "decimalToFloat64" "Convert a decimal to a float64." (fn Types.decimal Types.float64)
  ["decimalToFloat64(x) returns the IEEE 754 binary64 value closest to x under roundTiesToEven. Values\
  \ outside the float64 representable range overflow to \xB1\x221E. Subnormal precision loss is silent.",
   "Total."]

float32ToDecimal :: PrimitiveDefinition
float32ToDecimal = define "float32ToDecimal" "Convert a float32 to a decimal." (fn Types.float32 Types.decimal)
  ["float32ToDecimal(x) returns the exact decimal representation of the binary32 value x. Special\
  \ float32 values (\xB1\x221E, NaN) have a host-defined decimal representation; finite values convert\
  \ exactly since binary32 values are dyadic rationals.",
   "Total."]

float32ToFloat64 :: PrimitiveDefinition
float32ToFloat64 = define "float32ToFloat64" "Convert a float32 to a float64." (fn Types.float32 Types.float64)
  ["float32ToFloat64(x) returns the binary64 value with the same numeric value as x; \xB1\x221E and NaN\
  \ pass through. The conversion is exact and lossless.",
   "Total."]

float64ToDecimal :: PrimitiveDefinition
float64ToDecimal = define "float64ToDecimal" "Convert a float64 to a decimal." (fn Types.float64 Types.decimal)
  ["float64ToDecimal(x) returns the exact decimal representation of the binary64 value x. Special\
  \ float64 values (\xB1\x221E, NaN) have a host-defined decimal representation; finite values convert\
  \ exactly since binary64 values are dyadic rationals.",
   "Total."]

float64ToFloat32 :: PrimitiveDefinition
float64ToFloat32 = define "float64ToFloat32" "Convert a float64 to a float32 (lossy)." (fn Types.float64 Types.float32)
  ["float64ToFloat32(x) returns the binary32 value closest to x under roundTiesToEven. Values outside\
  \ the binary32 range overflow to \xB1\x221E; subnormal precision loss is silent; NaN payload may be\
  \ canonicalized by the host.",
   "Total."]

int16ToBigint :: PrimitiveDefinition
int16ToBigint = define "int16ToBigint" "Convert an int16 to a bigint." (fn Types.int16 Types.bigint)
  ["int16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

int32ToBigint :: PrimitiveDefinition
int32ToBigint = define "int32ToBigint" "Convert an int32 to a bigint." (fn Types.int32 Types.bigint)
  ["int32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

int64ToBigint :: PrimitiveDefinition
int64ToBigint = define "int64ToBigint" "Convert an int64 to a bigint." (fn Types.int64 Types.bigint)
  ["int64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

int8ToBigint :: PrimitiveDefinition
int8ToBigint = define "int8ToBigint" "Convert an int8 to a bigint." (fn Types.int8 Types.bigint)
  ["int8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

readBigint :: PrimitiveDefinition
readBigint = define "readBigint" "Parse a string as a bigint."
  (fn Types.string (Types.optional Types.bigint))
  ["readBigint(s) returns Just(x) where x is the bigint parsed from s, or Nothing if s is not a valid\
  \ bigint literal.",
   "Accepted syntax follows the Haskell Show/Read convention: an optional leading minus sign followed\
  \ by a sequence of decimal digits. Whitespace is not stripped; leading or trailing whitespace causes\
  \ parse failure.",
   "Total."]

readBoolean :: PrimitiveDefinition
readBoolean = define "readBoolean" "Parse a string as a boolean."
  (fn Types.string (Types.optional Types.boolean))
  ["readBoolean(s) returns Just(true) for \"true\", Just(false) for \"false\", and Nothing for any\
  \ other input (including \"True\"/\"False\" with capital initial letter on some hosts; behavior on\
  \ capitalized forms is host-defined and should not be relied upon).",
   "Total."]

readDecimal :: PrimitiveDefinition
readDecimal = define "readDecimal" "Parse a string as a decimal."
  (fn Types.string (Types.optional Types.decimal))
  ["readDecimal(s) returns Just(x) where x is the decimal parsed from s, or Nothing on parse failure.",
   "Accepted syntax is the standard decimal literal: an optional sign, an integer part, an optional\
  \ fractional part, and an optional exponent (e.g. -1.5, 2e10, 0.0).",
   "Total."]

readFloat32 :: PrimitiveDefinition
readFloat32 = define "readFloat32" "Parse a string as a float32."
  (fn Types.string (Types.optional Types.float32))
  ["readFloat32(s) returns Just(x) where x is the IEEE 754 binary32 value closest to the number parsed\
  \ from s under roundTiesToEven, or Nothing on parse failure.",
   "Accepted special-value literals (\"NaN\", \"Infinity\", \"-Infinity\", etc.) and their\
  \ capitalization are host-defined.",
   "Total."]

readFloat64 :: PrimitiveDefinition
readFloat64 = define "readFloat64" "Parse a string as a float64."
  (fn Types.string (Types.optional Types.float64))
  ["readFloat64(s) returns Just(x) where x is the IEEE 754 binary64 value closest to the number parsed\
  \ from s under roundTiesToEven, or Nothing on parse failure.",
   "Accepted special-value literals (\"NaN\", \"Infinity\", \"-Infinity\", etc.) and their\
  \ capitalization are host-defined.",
   "Total."]

readInt16 :: PrimitiveDefinition
readInt16 = define "readInt16" "Parse a string as an int16."
  (fn Types.string (Types.optional Types.int16))
  ["readInt16(s) returns Just(x) where x is the int16 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-2^15, 2^15-1].",
   "Total."]

readInt32 :: PrimitiveDefinition
readInt32 = define "readInt32" "Parse a string as an int32."
  (fn Types.string (Types.optional Types.int32))
  ["readInt32(s) returns Just(x) where x is the int32 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-2^31, 2^31-1].",
   "Total."]

readInt64 :: PrimitiveDefinition
readInt64 = define "readInt64" "Parse a string as an int64."
  (fn Types.string (Types.optional Types.int64))
  ["readInt64(s) returns Just(x) where x is the int64 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-2^63, 2^63-1].",
   "Total."]

readInt8 :: PrimitiveDefinition
readInt8 = define "readInt8" "Parse a string as an int8."
  (fn Types.string (Types.optional Types.int8))
  ["readInt8(s) returns Just(x) where x is the int8 parsed from s, or Nothing if s is not a valid\
  \ integer literal or the parsed value is outside [-128, 127].",
   "Total."]

readString :: PrimitiveDefinition
readString = define "readString"
  "Parse a string-literal token to a plain string (Just) or Nothing on parse failure."
  (fn Types.string (Types.optional Types.string))
  ["readString(s) parses s as a Haskell-syntax string literal token: an opening double-quote, escaped\
  \ characters per the Haskell lexical grammar (backslash escapes for special characters and Unicode\
  \ code points), and a closing double-quote. Returns Just of the decoded payload on success or\
  \ Nothing on malformed input.",
   "Total. The inverse of showString."]

readUint16 :: PrimitiveDefinition
readUint16 = define "readUint16" "Parse a string as a uint16."
  (fn Types.string (Types.optional Types.uint16))
  ["readUint16(s) returns Just(x) where x is the uint16 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 2^16-1].",
   "Total."]

readUint32 :: PrimitiveDefinition
readUint32 = define "readUint32" "Parse a string as a uint32."
  (fn Types.string (Types.optional Types.uint32))
  ["readUint32(s) returns Just(x) where x is the uint32 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 2^32-1].",
   "Total."]

readUint64 :: PrimitiveDefinition
readUint64 = define "readUint64" "Parse a string as a uint64."
  (fn Types.string (Types.optional Types.uint64))
  ["readUint64(s) returns Just(x) where x is the uint64 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 2^64-1].",
   "Total."]

readUint8 :: PrimitiveDefinition
readUint8 = define "readUint8" "Parse a string as a uint8."
  (fn Types.string (Types.optional Types.uint8))
  ["readUint8(s) returns Just(x) where x is the uint8 parsed from s, or Nothing if s is not a valid\
  \ non-negative integer literal or the parsed value is outside [0, 255].",
   "Total."]

showBigint :: PrimitiveDefinition
showBigint = define "showBigint" "Render a bigint as a string." (fn Types.bigint Types.string)
  ["showBigint(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits. No leading zeros are produced (except for the value 0 itself, which\
  \ renders as \"0\").",
   "Total. The inverse of readBigint."]

showBoolean :: PrimitiveDefinition
showBoolean = define "showBoolean" "Render a boolean as a string." (fn Types.boolean Types.string)
  ["showBoolean(true) = \"true\"; showBoolean(false) = \"false\".",
   "Total. The inverse of readBoolean."]

showDecimal :: PrimitiveDefinition
showDecimal = define "showDecimal" "Render a decimal as a string." (fn Types.decimal Types.string)
  ["showDecimal(x) returns the canonical decimal representation of x as a string. The exact form (use\
  \ of scientific notation, trailing zeros, etc.) is host-defined but always round-trips with\
  \ readDecimal.",
   "Total."]

showFloat32 :: PrimitiveDefinition
showFloat32 = define "showFloat32" "Render a float32 as a string." (fn Types.float32 Types.string)
  ["showFloat32(x) returns a string representation of x. Finite values use the shortest decimal that\
  \ round-trips through readFloat32 back to x. Special values render as \"NaN\", \"Infinity\", or\
  \ \"-Infinity\" (capitalization is host-defined).",
   "Total."]

showFloat64 :: PrimitiveDefinition
showFloat64 = define "showFloat64" "Render a float64 as a string." (fn Types.float64 Types.string)
  ["showFloat64(x) returns a string representation of x. Finite values use the shortest decimal that\
  \ round-trips through readFloat64 back to x. Special values render as \"NaN\", \"Infinity\", or\
  \ \"-Infinity\" (capitalization is host-defined).",
   "Total."]

showInt16 :: PrimitiveDefinition
showInt16 = define "showInt16" "Render an int16 as a string." (fn Types.int16 Types.string)
  ["showInt16(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of readInt16."]

showInt32 :: PrimitiveDefinition
showInt32 = define "showInt32" "Render an int32 as a string." (fn Types.int32 Types.string)
  ["showInt32(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of readInt32."]

showInt64 :: PrimitiveDefinition
showInt64 = define "showInt64" "Render an int64 as a string." (fn Types.int64 Types.string)
  ["showInt64(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of readInt64."]

showInt8 :: PrimitiveDefinition
showInt8 = define "showInt8" "Render an int8 as a string." (fn Types.int8 Types.string)
  ["showInt8(x) returns the canonical decimal representation of x: an optional leading minus sign\
  \ followed by decimal digits.",
   "Total. The inverse of readInt8."]

showString :: PrimitiveDefinition
showString = define "showString" "Render a string as a string-literal token (escaped and quoted)."
  (fn Types.string Types.string)
  ["showString(s) returns a Haskell-syntax string literal token representing s: an opening\
  \ double-quote, the characters of s with special characters escaped (backslash sequences for\
  \ control characters, quotes, and non-printable Unicode code points), and a closing double-quote.",
   "Total. The inverse of readString."]

showUint16 :: PrimitiveDefinition
showUint16 = define "showUint16" "Render a uint16 as a string." (fn Types.uint16 Types.string)
  ["showUint16(x) returns the canonical decimal representation of x.",
   "Total. The inverse of readUint16."]

showUint32 :: PrimitiveDefinition
showUint32 = define "showUint32" "Render a uint32 as a string." (fn Types.uint32 Types.string)
  ["showUint32(x) returns the canonical decimal representation of x.",
   "Total. The inverse of readUint32."]

showUint64 :: PrimitiveDefinition
showUint64 = define "showUint64" "Render a uint64 as a string." (fn Types.uint64 Types.string)
  ["showUint64(x) returns the canonical decimal representation of x.",
   "Total. The inverse of readUint64."]

showUint8 :: PrimitiveDefinition
showUint8 = define "showUint8" "Render a uint8 as a string." (fn Types.uint8 Types.string)
  ["showUint8(x) returns the canonical decimal representation of x.",
   "Total. The inverse of readUint8."]

stringToBinary :: PrimitiveDefinition
stringToBinary = define "stringToBinary" "Convert a UTF-8 string to binary data." (fn Types.string Types.binary)
  ["stringToBinary(s) encodes s as a UTF-8 byte sequence and returns the result as binary data.",
   "Total."]

uint16ToBigint :: PrimitiveDefinition
uint16ToBigint = define "uint16ToBigint" "Convert a uint16 to a bigint." (fn Types.uint16 Types.bigint)
  ["uint16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

uint32ToBigint :: PrimitiveDefinition
uint32ToBigint = define "uint32ToBigint" "Convert a uint32 to a bigint." (fn Types.uint32 Types.bigint)
  ["uint32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

uint64ToBigint :: PrimitiveDefinition
uint64ToBigint = define "uint64ToBigint" "Convert a uint64 to a bigint." (fn Types.uint64 Types.bigint)
  ["uint64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]

uint8ToBigint :: PrimitiveDefinition
uint8ToBigint = define "uint8ToBigint" "Convert a uint8 to a bigint." (fn Types.uint8 Types.bigint)
  ["uint8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
  \ lossless.",
   "Total."]
