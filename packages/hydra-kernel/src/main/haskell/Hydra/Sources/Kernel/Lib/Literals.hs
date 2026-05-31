-- | Primitive declarations for the hydra.lib.literals namespace.

module Hydra.Sources.Kernel.Lib.Literals where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.literals module.")}
  where
    definitions = [
      primNoDef "bigintToDecimal"  "Convert a bigint to a decimal." (sigFn Types.bigint Types.decimal) [
        "bigintToDecimal(x) returns the exact decimal representation of x. Since decimal is\
        \ arbitrary-precision and bigint is also arbitrary-precision, the conversion is lossless.",
        "Total."],
      primNoDef "bigintToInt16"    "Convert a bigint to an int16 (truncating)." (sigFn Types.bigint Types.int16) [
        "bigintToInt16(x) returns x reduced modulo 2^16 and reinterpreted as a signed two's-complement\
        \ int16. Values outside [-2^15, 2^15-1] wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToInt32"    "Convert a bigint to an int32 (truncating)." (sigFn Types.bigint Types.int32) [
        "bigintToInt32(x) returns x reduced modulo 2^32 and reinterpreted as a signed two's-complement\
        \ int32. Values outside [-2^31, 2^31-1] wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToInt64"    "Convert a bigint to an int64 (truncating)." (sigFn Types.bigint Types.int64) [
        "bigintToInt64(x) returns x reduced modulo 2^64 and reinterpreted as a signed two's-complement\
        \ int64. Values outside [-2^63, 2^63-1] wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToInt8"     "Convert a bigint to an int8 (truncating)." (sigFn Types.bigint Types.int8) [
        "bigintToInt8(x) returns x reduced modulo 2^8 and reinterpreted as a signed two's-complement\
        \ int8. Values outside [-128, 127] wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToUint16"   "Convert a bigint to a uint16 (truncating)." (sigFn Types.bigint Types.uint16) [
        "bigintToUint16(x) returns x reduced modulo 2^16 as an unsigned uint16. Values outside [0, 2^16-1]\
        \ wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToUint32"   "Convert a bigint to a uint32 (truncating)." (sigFn Types.bigint Types.uint32) [
        "bigintToUint32(x) returns x reduced modulo 2^32 as an unsigned uint32. Values outside [0, 2^32-1]\
        \ wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToUint64"   "Convert a bigint to a uint64 (truncating)." (sigFn Types.bigint Types.uint64) [
        "bigintToUint64(x) returns x reduced modulo 2^64 as an unsigned uint64. Values outside [0, 2^64-1]\
        \ wrap silently with no exception.",
        "Total."],
      primNoDef "bigintToUint8"    "Convert a bigint to a uint8 (truncating)." (sigFn Types.bigint Types.uint8) [
        "bigintToUint8(x) returns x reduced modulo 2^8 as an unsigned uint8. Values outside [0, 255] wrap\
        \ silently with no exception.",
        "Total."],
      primNoDef "binaryToBytes"    "Convert binary data to a list of byte values." (sigFn Types.binary (Types.list Types.int32)) [
        "binaryToBytes(b) returns the bytes of b as an int32 list with values in [0, 255], preserving byte\
        \ order.",
        "The result list element type is int32 rather than uint8 because Hydra's primitive collections work\
        \ most naturally with int32 indices.",
        "Total."],
      primNoDef "binaryToString"   "Convert binary data to a UTF-8 string." (sigFn Types.binary Types.string) [
        "binaryToString(b) interprets the bytes of b as a UTF-8-encoded string and returns the decoded\
        \ value.",
        "The behavior on invalid UTF-8 byte sequences is host-defined: most hosts substitute the replacement\
        \ character U+FFFD; some may signal an error or truncate.",
        "Total in the sense that it does not raise from the kernel's perspective, but the result may carry\
        \ the host's replacement semantics."],
      primNoDef "decimalToBigint"  "Convert a decimal to a bigint (truncating)." (sigFn Types.decimal Types.bigint) [
        "decimalToBigint(x) returns x truncated toward zero. Fractional parts are discarded.",
        "Total."],
      primNoDef "decimalToFloat32" "Convert a decimal to a float32." (sigFn Types.decimal Types.float32) [
        "decimalToFloat32(x) returns the IEEE 754 binary32 value closest to x under roundTiesToEven. Values\
        \ outside the float32 representable range overflow to \xB1\x221E. Subnormal precision loss is silent.",
        "Total."],
      primNoDef "decimalToFloat64" "Convert a decimal to a float64." (sigFn Types.decimal Types.float64) [
        "decimalToFloat64(x) returns the IEEE 754 binary64 value closest to x under roundTiesToEven. Values\
        \ outside the float64 representable range overflow to \xB1\x221E. Subnormal precision loss is silent.",
        "Total."],
      primNoDef "float32ToDecimal" "Convert a float32 to a decimal." (sigFn Types.float32 Types.decimal) [
        "float32ToDecimal(x) returns the exact decimal representation of the binary32 value x. Special\
        \ float32 values (\xB1\x221E, NaN) have a host-defined decimal representation; finite values convert\
        \ exactly since binary32 values are dyadic rationals.",
        "Total."],
      primNoDef "float32ToFloat64" "Convert a float32 to a float64." (sigFn Types.float32 Types.float64) [
        "float32ToFloat64(x) returns the binary64 value with the same numeric value as x; \xB1\x221E and NaN\
        \ pass through. The conversion is exact and lossless.",
        "Total."],
      primNoDef "float64ToDecimal" "Convert a float64 to a decimal." (sigFn Types.float64 Types.decimal) [
        "float64ToDecimal(x) returns the exact decimal representation of the binary64 value x. Special\
        \ float64 values (\xB1\x221E, NaN) have a host-defined decimal representation; finite values convert\
        \ exactly since binary64 values are dyadic rationals.",
        "Total."],
      primNoDef "float64ToFloat32" "Convert a float64 to a float32 (lossy)." (sigFn Types.float64 Types.float32) [
        "float64ToFloat32(x) returns the binary32 value closest to x under roundTiesToEven. Values outside\
        \ the binary32 range overflow to \xB1\x221E; subnormal precision loss is silent; NaN payload may be\
        \ canonicalized by the host.",
        "Total."],
      primNoDef "int16ToBigint"    "Convert an int16 to a bigint." (sigFn Types.int16 Types.bigint) [
        "int16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "int32ToBigint"    "Convert an int32 to a bigint." (sigFn Types.int32 Types.bigint) [
        "int32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "int64ToBigint"    "Convert an int64 to a bigint." (sigFn Types.int64 Types.bigint) [
        "int64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "int8ToBigint"     "Convert an int8 to a bigint." (sigFn Types.int8 Types.bigint) [
        "int8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "readBigint"       "Parse a string as a bigint." (sigFn Types.string (Types.optional Types.bigint)) [
        "readBigint(s) returns Just(x) where x is the bigint parsed from s, or Nothing if s is not a valid\
        \ bigint literal.",
        "Accepted syntax follows the Haskell Show/Read convention: an optional leading minus sign followed\
        \ by a sequence of decimal digits. Whitespace is not stripped; leading or trailing whitespace causes\
        \ parse failure.",
        "Total."],
      primNoDef "readBoolean"      "Parse a string as a boolean." (sigFn Types.string (Types.optional Types.boolean)) [
        "readBoolean(s) returns Just(true) for \"true\", Just(false) for \"false\", and Nothing for any\
        \ other input (including \"True\"/\"False\" with capital initial letter on some hosts; behavior on\
        \ capitalized forms is host-defined and should not be relied upon).",
        "Total."],
      primNoDef "readDecimal"      "Parse a string as a decimal." (sigFn Types.string (Types.optional Types.decimal)) [
        "readDecimal(s) returns Just(x) where x is the decimal parsed from s, or Nothing on parse failure.",
        "Accepted syntax is the standard decimal literal: an optional sign, an integer part, an optional\
        \ fractional part, and an optional exponent (e.g. -1.5, 2e10, 0.0).",
        "Total."],
      primNoDef "readFloat32"      "Parse a string as a float32." (sigFn Types.string (Types.optional Types.float32)) [
        "readFloat32(s) returns Just(x) where x is the IEEE 754 binary32 value closest to the number parsed\
        \ from s under roundTiesToEven, or Nothing on parse failure.",
        "Accepted special-value literals (\"NaN\", \"Infinity\", \"-Infinity\", etc.) and their\
        \ capitalization are host-defined.",
        "Total."],
      primNoDef "readFloat64"      "Parse a string as a float64." (sigFn Types.string (Types.optional Types.float64)) [
        "readFloat64(s) returns Just(x) where x is the IEEE 754 binary64 value closest to the number parsed\
        \ from s under roundTiesToEven, or Nothing on parse failure.",
        "Accepted special-value literals (\"NaN\", \"Infinity\", \"-Infinity\", etc.) and their\
        \ capitalization are host-defined.",
        "Total."],
      primNoDef "readInt16"        "Parse a string as an int16." (sigFn Types.string (Types.optional Types.int16)) [
        "readInt16(s) returns Just(x) where x is the int16 parsed from s, or Nothing if s is not a valid\
        \ integer literal or the parsed value is outside [-2^15, 2^15-1].",
        "Total."],
      primNoDef "readInt32"        "Parse a string as an int32." (sigFn Types.string (Types.optional Types.int32)) [
        "readInt32(s) returns Just(x) where x is the int32 parsed from s, or Nothing if s is not a valid\
        \ integer literal or the parsed value is outside [-2^31, 2^31-1].",
        "Total."],
      primNoDef "readInt64"        "Parse a string as an int64." (sigFn Types.string (Types.optional Types.int64)) [
        "readInt64(s) returns Just(x) where x is the int64 parsed from s, or Nothing if s is not a valid\
        \ integer literal or the parsed value is outside [-2^63, 2^63-1].",
        "Total."],
      primNoDef "readInt8"         "Parse a string as an int8." (sigFn Types.string (Types.optional Types.int8)) [
        "readInt8(s) returns Just(x) where x is the int8 parsed from s, or Nothing if s is not a valid\
        \ integer literal or the parsed value is outside [-128, 127].",
        "Total."],
      primNoDef "readString"       "Parse a string-literal token to a plain string (Just) or Nothing on parse failure." (sigFn Types.string (Types.optional Types.string)) [
        "readString(s) parses s as a Haskell-syntax string literal token: an opening double-quote, escaped\
        \ characters per the Haskell lexical grammar (backslash escapes for special characters and Unicode\
        \ code points), and a closing double-quote. Returns Just of the decoded payload on success or\
        \ Nothing on malformed input.",
        "Total. The inverse of showString."],
      primNoDef "readUint16"       "Parse a string as a uint16." (sigFn Types.string (Types.optional Types.uint16)) [
        "readUint16(s) returns Just(x) where x is the uint16 parsed from s, or Nothing if s is not a valid\
        \ non-negative integer literal or the parsed value is outside [0, 2^16-1].",
        "Total."],
      primNoDef "readUint32"       "Parse a string as a uint32." (sigFn Types.string (Types.optional Types.uint32)) [
        "readUint32(s) returns Just(x) where x is the uint32 parsed from s, or Nothing if s is not a valid\
        \ non-negative integer literal or the parsed value is outside [0, 2^32-1].",
        "Total."],
      primNoDef "readUint64"       "Parse a string as a uint64." (sigFn Types.string (Types.optional Types.uint64)) [
        "readUint64(s) returns Just(x) where x is the uint64 parsed from s, or Nothing if s is not a valid\
        \ non-negative integer literal or the parsed value is outside [0, 2^64-1].",
        "Total."],
      primNoDef "readUint8"        "Parse a string as a uint8." (sigFn Types.string (Types.optional Types.uint8)) [
        "readUint8(s) returns Just(x) where x is the uint8 parsed from s, or Nothing if s is not a valid\
        \ non-negative integer literal or the parsed value is outside [0, 255].",
        "Total."],
      primNoDef "showBigint"       "Render a bigint as a string." (sigFn Types.bigint Types.string) [
        "showBigint(x) returns the canonical decimal representation of x: an optional leading minus sign\
        \ followed by decimal digits. No leading zeros are produced (except for the value 0 itself, which\
        \ renders as \"0\").",
        "Total. The inverse of readBigint."],
      primNoDef "showBoolean"      "Render a boolean as a string." (sigFn Types.boolean Types.string) [
        "showBoolean(true) = \"true\"; showBoolean(false) = \"false\".",
        "Total. The inverse of readBoolean."],
      primNoDef "showDecimal"      "Render a decimal as a string." (sigFn Types.decimal Types.string) [
        "showDecimal(x) returns the canonical decimal representation of x as a string. The exact form (use\
        \ of scientific notation, trailing zeros, etc.) is host-defined but always round-trips with\
        \ readDecimal.",
        "Total."],
      primNoDef "showFloat32"      "Render a float32 as a string." (sigFn Types.float32 Types.string) [
        "showFloat32(x) returns a string representation of x. Finite values use the shortest decimal that\
        \ round-trips through readFloat32 back to x. Special values render as \"NaN\", \"Infinity\", or\
        \ \"-Infinity\" (capitalization is host-defined).",
        "Total."],
      primNoDef "showFloat64"      "Render a float64 as a string." (sigFn Types.float64 Types.string) [
        "showFloat64(x) returns a string representation of x. Finite values use the shortest decimal that\
        \ round-trips through readFloat64 back to x. Special values render as \"NaN\", \"Infinity\", or\
        \ \"-Infinity\" (capitalization is host-defined).",
        "Total."],
      primNoDef "showInt16"        "Render an int16 as a string." (sigFn Types.int16 Types.string) [
        "showInt16(x) returns the canonical decimal representation of x: an optional leading minus sign\
        \ followed by decimal digits.",
        "Total. The inverse of readInt16."],
      primNoDef "showInt32"        "Render an int32 as a string." (sigFn Types.int32 Types.string) [
        "showInt32(x) returns the canonical decimal representation of x: an optional leading minus sign\
        \ followed by decimal digits.",
        "Total. The inverse of readInt32."],
      primNoDef "showInt64"        "Render an int64 as a string." (sigFn Types.int64 Types.string) [
        "showInt64(x) returns the canonical decimal representation of x: an optional leading minus sign\
        \ followed by decimal digits.",
        "Total. The inverse of readInt64."],
      primNoDef "showInt8"         "Render an int8 as a string." (sigFn Types.int8 Types.string) [
        "showInt8(x) returns the canonical decimal representation of x: an optional leading minus sign\
        \ followed by decimal digits.",
        "Total. The inverse of readInt8."],
      primNoDef "showString"       "Render a string as a string-literal token (escaped and quoted)." (sigFn Types.string Types.string) [
        "showString(s) returns a Haskell-syntax string literal token representing s: an opening\
        \ double-quote, the characters of s with special characters escaped (backslash sequences for\
        \ control characters, quotes, and non-printable Unicode code points), and a closing double-quote.",
        "Total. The inverse of readString."],
      primNoDef "showUint16"       "Render a uint16 as a string." (sigFn Types.uint16 Types.string) [
        "showUint16(x) returns the canonical decimal representation of x.",
        "Total. The inverse of readUint16."],
      primNoDef "showUint32"       "Render a uint32 as a string." (sigFn Types.uint32 Types.string) [
        "showUint32(x) returns the canonical decimal representation of x.",
        "Total. The inverse of readUint32."],
      primNoDef "showUint64"       "Render a uint64 as a string." (sigFn Types.uint64 Types.string) [
        "showUint64(x) returns the canonical decimal representation of x.",
        "Total. The inverse of readUint64."],
      primNoDef "showUint8"        "Render a uint8 as a string." (sigFn Types.uint8 Types.string) [
        "showUint8(x) returns the canonical decimal representation of x.",
        "Total. The inverse of readUint8."],
      primNoDef "stringToBinary"   "Convert a UTF-8 string to binary data." (sigFn Types.string Types.binary) [
        "stringToBinary(s) encodes s as a UTF-8 byte sequence and returns the result as binary data.",
        "Total."],
      primNoDef "uint16ToBigint"   "Convert a uint16 to a bigint." (sigFn Types.uint16 Types.bigint) [
        "uint16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "uint32ToBigint"   "Convert a uint32 to a bigint." (sigFn Types.uint32 Types.bigint) [
        "uint32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "uint64ToBigint"   "Convert a uint64 to a bigint." (sigFn Types.uint64 Types.bigint) [
        "uint64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."],
      primNoDef "uint8ToBigint"    "Convert a uint8 to a bigint." (sigFn Types.uint8 Types.bigint) [
        "uint8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and\
        \ lossless.",
        "Total."]]

primNoDef :: String -> String -> TermSignature -> [String] -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Build a monomorphic signature `a -> b`.
sigFn :: Type -> Type -> TermSignature
sigFn a b = sig $ TypeScheme [] (a Types.~> b) Nothing
