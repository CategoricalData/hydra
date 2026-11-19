<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.literals

Conversion and parse/print for literal values.

The numeric conversion primitives (bigintToInt32, float64ToFloat32, ...) convert between Hydra's literal numeric types; lossless conversions are exact, and each lossy conversion states its truncation or rounding behavior in its own documentation.

The parseX primitives are pure conversions from text to values, with an optional codomain wherever parsing can fail; the printX primitives are the pure conversions back from values to text.

## base64ToBinary

Decode a base64 ASCII string to binary data.

`base64ToBinary(s: string) → binary`

**Parameters**

- `s` (`string`): the base64 string to decode

**Returns** `binary`

*Since: 0.18.*

base64ToBinary(s) decodes the standard base64 (RFC 4648) ASCII string s and returns the resulting bytes as binary data.

Inverse of binaryToBase64: base64ToBinary(binaryToBase64(b)) is b for every binary value. Behavior on strings which are not valid base64 is host-defined.

Renamed from hydra.lib.literals.stringToBinary.

**[HYDRA-CORE-LIB-LITERALS-BASE64-TO-BINARY-DECODES-STANDARD-BASE64] (requirement)** base64ToBinary decodes a standard base64 (RFC 4648) ASCII string to the binary data it represents, including the empty string decoding to empty binary data.

## bigintToDecimal

Convert a bigint to a decimal.

`bigintToDecimal(x: bigint) → decimal`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `decimal`

*Since: 0.15.*

bigintToDecimal(x) returns the exact decimal representation of x. Since decimal is arbitrary-precision and bigint is also arbitrary-precision, the conversion is lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-DECIMAL-LOSSLESS-CONVERSION] (requirement)** bigintToDecimal returns the exact decimal representation of a bigint, with no precision loss, for positive, negative, and zero values.

## bigintToInt16

Convert a bigint to an int16 (truncating).

`bigintToInt16(x: bigint) → int16`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `int16`

*Since: 0.15.*

bigintToInt16(x) returns x reduced modulo 2^16 and reinterpreted as a signed two's-complement int16. Values outside [-2^15, 2^15-1] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT16-CONVERTS-IN-RANGE] (requirement)** bigintToInt16 returns the int16 with the same numeric value as the bigint, for positive and negative values within the int16 range.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT16-WRAPS-TWOS-COMPLEMENT-OUT-OF-RANGE] (requirement)** bigintToInt16 on a value outside the int16 range ([-2^15, 2^15-1]) wraps silently to the corresponding two's-complement int16, with no exception, at both the positive and negative (all-bits-set) boundary.

## bigintToInt32

Convert a bigint to an int32 (truncating).

`bigintToInt32(x: bigint) → int32`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `int32`

*Since: 0.15.*

bigintToInt32(x) returns x reduced modulo 2^32 and reinterpreted as a signed two's-complement int32. Values outside [-2^31, 2^31-1] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT32-CONVERTS-IN-RANGE] (requirement)** bigintToInt32 returns the int32 with the same numeric value as the bigint, for positive and negative values within the int32 range.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT32-WRAPS-TWOS-COMPLEMENT-OUT-OF-RANGE] (requirement)** bigintToInt32 on a value outside the int32 range ([-2^31, 2^31-1]) wraps silently to the corresponding two's-complement int32, with no exception, at both the positive and negative (all-bits-set) boundary; this holds even on hosts where int32 is represented as a wider native integer, which must not skip the narrowing step.

## bigintToInt64

Convert a bigint to an int64 (truncating).

`bigintToInt64(x: bigint) → int64`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `int64`

*Since: 0.15.*

bigintToInt64(x) returns x reduced modulo 2^64 and reinterpreted as a signed two's-complement int64. Values outside [-2^63, 2^63-1] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT64-CONVERTS-IN-RANGE] (requirement)** bigintToInt64 returns the int64 with the same numeric value as the bigint, for positive and negative values within the int64 range.

## bigintToInt8

Convert a bigint to an int8 (truncating).

`bigintToInt8(x: bigint) → int8`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `int8`

*Since: 0.15.*

bigintToInt8(x) returns x reduced modulo 2^8 and reinterpreted as a signed two's-complement int8. Values outside [-128, 127] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT8-CONVERTS-IN-RANGE] (requirement)** bigintToInt8 returns the int8 with the same numeric value as the bigint, for positive and negative values within the int8 range.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-INT8-WRAPS-TWOS-COMPLEMENT-OUT-OF-RANGE] (requirement)** bigintToInt8 on a value outside the int8 range ([-128, 127]) wraps silently to the corresponding two's-complement int8, with no exception, at both the positive and negative (all-bits-set) boundary.

## bigintToUint16

Convert a bigint to a uint16 (truncating).

`bigintToUint16(x: bigint) → uint16`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `uint16`

*Since: 0.15.*

bigintToUint16(x) returns x reduced modulo 2^16 as an unsigned uint16. Values outside [0, 2^16-1] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-UINT16-CONVERTS-IN-RANGE] (requirement)** bigintToUint16 returns the uint16 with the same numeric value as the bigint, for a zero or typical in-range value.

## bigintToUint32

Convert a bigint to a uint32 (truncating).

`bigintToUint32(x: bigint) → uint32`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `uint32`

*Since: 0.15.*

bigintToUint32(x) returns x reduced modulo 2^32 as an unsigned uint32. Values outside [0, 2^32-1] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-UINT32-CONVERTS-IN-RANGE] (requirement)** bigintToUint32 returns the uint32 with the same numeric value as the bigint, for a zero or typical in-range value.

## bigintToUint64

Convert a bigint to a uint64 (truncating).

`bigintToUint64(x: bigint) → uint64`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `uint64`

*Since: 0.15.*

bigintToUint64(x) returns x reduced modulo 2^64 as an unsigned uint64. Values outside [0, 2^64-1] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-UINT64-CONVERTS-IN-RANGE] (requirement)** bigintToUint64 returns the uint64 with the same numeric value as the bigint, for a zero or typical in-range value.

## bigintToUint8

Convert a bigint to a uint8 (truncating).

`bigintToUint8(x: bigint) → uint8`

**Parameters**

- `x` (`bigint`): the bigint to convert

**Returns** `uint8`

*Since: 0.15.*

bigintToUint8(x) returns x reduced modulo 2^8 as an unsigned uint8. Values outside [0, 255] wrap silently with no exception.

Total.

**[HYDRA-CORE-LIB-LITERALS-BIGINT-TO-UINT8-CONVERTS-IN-RANGE] (requirement)** bigintToUint8 returns the uint8 with the same numeric value as the bigint, for a zero or typical in-range value.

## binaryToBase64

Encode binary data as a base64 ASCII string.

`binaryToBase64(b: binary) → string`

**Parameters**

- `b` (`binary`): the binary data to encode

**Returns** `string`

*Since: 0.18.*

binaryToBase64(b) encodes the bytes of b using standard base64 (RFC 4648) and returns the result as an ASCII string.

Total and round-trippable: base64ToBinary(binaryToBase64(b)) is b for every binary value. Use this to embed arbitrary binary data in a string context (e.g. a JSON value). For interpreting bytes as text, use hydra.core.lib.text.decodeUtf8 instead.

Renamed from hydra.lib.literals.binaryToString.

**[HYDRA-CORE-LIB-LITERALS-BINARY-TO-BASE64-ENCODES-STANDARD-BASE64] (requirement)** binaryToBase64 encodes binary data as a standard base64 (RFC 4648) ASCII string, including empty binary data encoding to the empty string.

## binaryToBytes

Convert binary data to a list of byte values.

`binaryToBytes(b: binary) → list<int32>`

**Parameters**

- `b` (`binary`): the binary data to convert

**Returns** `list<int32>`

*Since: 0.15.*

binaryToBytes(b) returns the bytes of b as an int32 list with values in [0, 255], preserving byte order.

The result list element type is int32 rather than uint8 because Hydra's primitive collections work most naturally with int32 indices.

Total.

**[HYDRA-CORE-LIB-LITERALS-BINARY-TO-BYTES-RETURNS-UNSIGNED-BYTE-VALUES] (requirement)** binaryToBytes returns the bytes of the binary data as a list of int32 values in [0, 255], preserving byte order, including byte values above 127 (which remain unsigned, not reinterpreted as negative), and the empty list for empty binary data.

## decimalToBigint

Convert a decimal to a bigint (truncating).

`decimalToBigint(x: decimal) → bigint`

**Parameters**

- `x` (`decimal`): the decimal to convert

**Returns** `bigint`

*Since: 0.15.*

decimalToBigint(x) returns x truncated toward zero. Fractional parts are discarded.

Total.

**[HYDRA-CORE-LIB-LITERALS-DECIMAL-TO-BIGINT-CONVERTS-WHOLE-NUMBERS] (requirement)** decimalToBigint returns the bigint with the same numeric value as a whole-number decimal, for positive, negative, and zero values.

**[HYDRA-CORE-LIB-LITERALS-DECIMAL-TO-BIGINT-TRUNCATES-FRACTION-TOWARD-ZERO] (requirement)** decimalToBigint on a decimal with a fractional part discards the fraction, truncating toward zero (rounding down for positive values, up for negative values), not rounding to nearest.

## decimalToFloat32

Convert a decimal to a float32.

`decimalToFloat32(x: decimal) → float32`

**Parameters**

- `x` (`decimal`): the decimal to convert

**Returns** `float32`

*Since: 0.15.*

decimalToFloat32(x) returns the IEEE 754 binary32 value closest to x under roundTiesToEven. Values outside the float32 representable range overflow to ±∞. Subnormal precision loss is silent.

Total.

**[HYDRA-CORE-LIB-LITERALS-DECIMAL-TO-FLOAT32-CONVERTS-EXACTLY-REPRESENTABLE-VALUES] (requirement)** decimalToFloat32 returns the float32 value with the same numeric value as a decimal that is exactly representable in binary32, for zero and whole-number values.

## decimalToFloat64

Convert a decimal to a float64.

`decimalToFloat64(x: decimal) → float64`

**Parameters**

- `x` (`decimal`): the decimal to convert

**Returns** `float64`

*Since: 0.15.*

decimalToFloat64(x) returns the IEEE 754 binary64 value closest to x under roundTiesToEven. Values outside the float64 representable range overflow to ±∞. Subnormal precision loss is silent.

Total.

**[HYDRA-CORE-LIB-LITERALS-DECIMAL-TO-FLOAT64-CONVERTS-EXACTLY-REPRESENTABLE-VALUES] (requirement)** decimalToFloat64 returns the float64 value with the same numeric value as a decimal that is exactly representable in binary64, for zero and whole-number values.

## float32ToDecimal

Convert a float32 to a decimal.

`float32ToDecimal(x: float32) → decimal`

**Parameters**

- `x` (`float32`): the float32 to convert

**Returns** `decimal`

*Since: 0.15.*

float32ToDecimal(x) returns the exact decimal representation of the binary32 value x. Special float32 values (±∞, NaN) have a host-defined decimal representation; finite values convert exactly since binary32 values are dyadic rationals.

Total.

**[HYDRA-CORE-LIB-LITERALS-FLOAT32-TO-DECIMAL-CONVERTS-EXACTLY-REPRESENTABLE-VALUES] (requirement)** float32ToDecimal returns the exact decimal representation of a binary32 value, for zero and whole-number values.

## float32ToFloat64

Convert a float32 to a float64.

`float32ToFloat64(x: float32) → float64`

**Parameters**

- `x` (`float32`): the float32 to convert

**Returns** `float64`

*Since: 0.15.*

float32ToFloat64(x) returns the binary64 value with the same numeric value as x; ±∞ and NaN pass through. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-FLOAT32-TO-FLOAT64-LOSSLESS-WIDENING] (requirement)** float32ToFloat64 returns the float64 value with the same numeric value as a float32, with no precision loss, for positive, negative, and zero values.

## float64ToDecimal

Convert a float64 to a decimal.

`float64ToDecimal(x: float64) → decimal`

**Parameters**

- `x` (`float64`): the float64 to convert

**Returns** `decimal`

*Since: 0.15.*

float64ToDecimal(x) returns the exact decimal representation of the binary64 value x. Special float64 values (±∞, NaN) have a host-defined decimal representation; finite values convert exactly since binary64 values are dyadic rationals.

Total.

**[HYDRA-CORE-LIB-LITERALS-FLOAT64-TO-DECIMAL-CONVERTS-EXACTLY-REPRESENTABLE-VALUES] (requirement)** float64ToDecimal returns the exact decimal representation of a binary64 value, for zero and whole-number values.

## float64ToFloat32

Convert a float64 to a float32 (lossy).

`float64ToFloat32(x: float64) → float32`

**Parameters**

- `x` (`float64`): the float64 to convert

**Returns** `float32`

*Since: 0.15.*

float64ToFloat32(x) returns the binary32 value closest to x under roundTiesToEven. Values outside the binary32 range overflow to ±∞; subnormal precision loss is silent; NaN payload may be canonicalized by the host.

Total.

**[HYDRA-CORE-LIB-LITERALS-FLOAT64-TO-FLOAT32-CONVERTS-EXACTLY-REPRESENTABLE-VALUES] (requirement)** float64ToFloat32 returns the float32 value with the same numeric value as a float64 that is exactly representable in binary32, for positive, negative, and zero values.

## int16ToBigint

Convert an int16 to a bigint.

`int16ToBigint(x: int16) → bigint`

**Parameters**

- `x` (`int16`): the int16 to convert

**Returns** `bigint`

*Since: 0.15.*

int16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-INT16-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** int16ToBigint returns the bigint with the same numeric value as the int16, with no precision loss, for positive and negative values.

## int32ToBigint

Convert an int32 to a bigint.

`int32ToBigint(x: int32) → bigint`

**Parameters**

- `x` (`int32`): the int32 to convert

**Returns** `bigint`

*Since: 0.15.*

int32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-INT32-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** int32ToBigint returns the bigint with the same numeric value as the int32, with no precision loss, for positive, negative, and zero values.

## int64ToBigint

Convert an int64 to a bigint.

`int64ToBigint(x: int64) → bigint`

**Parameters**

- `x` (`int64`): the int64 to convert

**Returns** `bigint`

*Since: 0.15.*

int64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-INT64-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** int64ToBigint returns the bigint with the same numeric value as the int64, with no precision loss, for positive and negative values.

## int8ToBigint

Convert an int8 to a bigint.

`int8ToBigint(x: int8) → bigint`

**Parameters**

- `x` (`int8`): the int8 to convert

**Returns** `bigint`

*Since: 0.15.*

int8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-INT8-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** int8ToBigint returns the bigint with the same numeric value as the int8, with no precision loss, for positive and negative values including the int8 min/max boundary.

## parseBigint

Parse a string as a bigint.

`parseBigint(s: string) → optional<bigint>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<bigint>`

*Since: 0.17.6.*

parseBigint(s) returns Just(x) where x is the bigint parsed from s, or Nothing if s is not a valid bigint literal.

Accepted syntax follows the Haskell Show/Read convention: an optional leading minus sign followed by a sequence of decimal digits. Whitespace is not stripped; leading or trailing whitespace causes parse failure.

Total.

Renamed from hydra.lib.literals.readBigint.

**[HYDRA-CORE-LIB-LITERALS-PARSE-BIGINT-PARSES-VALID-LITERALS] (requirement)** parseBigint returns the bigint parsed from a valid bigint literal, wrapped, for positive, negative, zero, and arbitrarily large values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-BIGINT-NONE-ON-INVALID-INPUT] (requirement)** parseBigint returns none when the string is not a valid bigint literal.

## parseBoolean

Parse a string as a boolean.

`parseBoolean(s: string) → optional<boolean>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<boolean>`

*Since: 0.17.6.*

parseBoolean(s) returns Just(true) for "true", Just(false) for "false", and Nothing for any other input (including "True"/"False" with capital initial letter on some hosts; behavior on capitalized forms is host-defined and should not be relied upon).

Total.

Renamed from hydra.lib.literals.readBoolean.

**[HYDRA-CORE-LIB-LITERALS-PARSE-BOOLEAN-PARSES-EXACT-LOWERCASE-LITERALS] (requirement)** parseBoolean returns the wrapped boolean true for the exact string "true" and false for "false".

**[HYDRA-CORE-LIB-LITERALS-PARSE-BOOLEAN-NONE-ON-INVALID-INPUT] (requirement)** parseBoolean returns none for any string other than the exact lowercase literals "true" or "false".

## parseDecimal

Parse a string as a decimal.

`parseDecimal(s: string) → optional<decimal>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<decimal>`

*Since: 0.17.6.*

parseDecimal(s) returns Just(x) where x is the decimal parsed from s, or Nothing on parse failure.

Accepted syntax is the standard decimal literal: an optional sign, an integer part, an optional fractional part, and an optional exponent (e.g. -1.5, 2e10, 0.0).

Total.

Renamed from hydra.lib.literals.readDecimal.

**[HYDRA-CORE-LIB-LITERALS-PARSE-DECIMAL-PARSES-VALID-LITERALS] (requirement)** parseDecimal returns the decimal parsed from a valid decimal literal, wrapped, for positive, negative, and zero values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-DECIMAL-NONE-ON-INVALID-INPUT] (requirement)** parseDecimal returns none when the string is not a valid decimal literal.

## parseFloat32

Parse a string as a float32.

`parseFloat32(s: string) → optional<float32>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<float32>`

*Since: 0.17.6.*

parseFloat32(s) returns Just(x) where x is the IEEE 754 binary32 value closest to the number parsed from s under roundTiesToEven, or Nothing on parse failure.

Accepted special-value literals ("NaN", "Infinity", "-Infinity", etc.) and their capitalization are host-defined.

Total.

Renamed from hydra.lib.literals.readFloat32.

**[HYDRA-CORE-LIB-LITERALS-PARSE-FLOAT32-PARSES-VALID-LITERALS] (requirement)** parseFloat32 returns the float32 value closest to the number parsed from a valid literal, wrapped, for positive and negative values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-FLOAT32-NONE-ON-INVALID-INPUT] (requirement)** parseFloat32 returns none when the string is not a valid float literal.

## parseFloat64

Parse a string as a float64.

`parseFloat64(s: string) → optional<float64>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<float64>`

*Since: 0.17.6.*

parseFloat64(s) returns Just(x) where x is the IEEE 754 binary64 value closest to the number parsed from s under roundTiesToEven, or Nothing on parse failure.

Accepted special-value literals ("NaN", "Infinity", "-Infinity", etc.) and their capitalization are host-defined.

Total.

Renamed from hydra.lib.literals.readFloat64.

**[HYDRA-CORE-LIB-LITERALS-PARSE-FLOAT64-PARSES-VALID-LITERALS] (requirement)** parseFloat64 returns the float64 value closest to the number parsed from a valid literal, wrapped, for positive and negative values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-FLOAT64-NONE-ON-INVALID-INPUT] (requirement)** parseFloat64 returns none when the string is not a valid float literal.

## parseInt16

Parse a string as an int16.

`parseInt16(s: string) → optional<int16>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<int16>`

*Since: 0.17.6.*

parseInt16(s) returns Just(x) where x is the int16 parsed from s, or Nothing if s is not a valid integer literal or the parsed value is outside [-2^15, 2^15-1].

Total.

Renamed from hydra.lib.literals.readInt16.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT16-PARSES-VALID-LITERALS] (requirement)** parseInt16 returns the int16 parsed from a valid integer literal within range, wrapped, for positive and negative values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT16-NONE-ON-INVALID-INPUT] (requirement)** parseInt16 returns none when the string is not a valid integer literal.

## parseInt32

Parse a string as an int32.

`parseInt32(s: string) → optional<int32>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<int32>`

*Since: 0.17.6.*

parseInt32(s) returns Just(x) where x is the int32 parsed from s, or Nothing if s is not a valid integer literal or the parsed value is outside [-2^31, 2^31-1].

Total.

Renamed from hydra.lib.literals.readInt32.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT32-PARSES-VALID-LITERALS] (requirement)** parseInt32 returns the int32 parsed from a valid integer literal within range, wrapped, for positive and negative values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT32-NONE-ON-INVALID-INPUT] (requirement)** parseInt32 returns none when the string is not a valid integer literal.

## parseInt64

Parse a string as an int64.

`parseInt64(s: string) → optional<int64>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<int64>`

*Since: 0.17.6.*

parseInt64(s) returns Just(x) where x is the int64 parsed from s, or Nothing if s is not a valid integer literal or the parsed value is outside [-2^63, 2^63-1].

Total.

Renamed from hydra.lib.literals.readInt64.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT64-PARSES-VALID-LITERALS] (requirement)** parseInt64 returns the int64 parsed from a valid integer literal within range, wrapped, for positive and negative values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT64-NONE-ON-INVALID-INPUT] (requirement)** parseInt64 returns none when the string is not a valid integer literal.

## parseInt8

Parse a string as an int8.

`parseInt8(s: string) → optional<int8>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<int8>`

*Since: 0.17.6.*

parseInt8(s) returns Just(x) where x is the int8 parsed from s, or Nothing if s is not a valid integer literal or the parsed value is outside [-128, 127].

Total.

Renamed from hydra.lib.literals.readInt8.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT8-PARSES-VALID-LITERALS] (requirement)** parseInt8 returns the int8 parsed from a valid integer literal within range, wrapped, for positive and negative values including the int8 min/max boundary.

**[HYDRA-CORE-LIB-LITERALS-PARSE-INT8-NONE-ON-OUT-OF-RANGE-OR-INVALID-INPUT] (requirement)** parseInt8 returns none when the string is not a valid integer literal, or when the parsed value is outside the int8 range [-128, 127].

## parseString

Parse a string-literal token to a plain string (Just) or Nothing on parse failure.

`parseString(s: string) → optional<string>`

**Parameters**

- `s` (`string`): the string-literal token to parse

**Returns** `optional<string>`

*Since: 0.17.6.*

parseString(s) parses s as a Haskell-syntax string literal token: an opening double-quote, escaped characters per the Haskell lexical grammar (backslash escapes for special characters and Unicode code points), and a closing double-quote. Returns Just of the decoded payload on success or Nothing on malformed input.

Total. The inverse of printString.

Renamed from hydra.lib.literals.readString.

**[HYDRA-CORE-LIB-LITERALS-PARSE-STRING-PARSES-QUOTED-LITERALS] (requirement)** parseString returns the decoded payload of a double-quoted string literal, wrapped.

**[HYDRA-CORE-LIB-LITERALS-PARSE-STRING-NONE-ON-UNQUOTED-INPUT] (requirement)** parseString returns none when the string is not a well-formed double-quoted string literal (for example, an unquoted bare string).

## parseUint16

Parse a string as a uint16.

`parseUint16(s: string) → optional<uint16>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<uint16>`

*Since: 0.17.6.*

parseUint16(s) returns Just(x) where x is the uint16 parsed from s, or Nothing if s is not a valid non-negative integer literal or the parsed value is outside [0, 2^16-1].

Total.

Renamed from hydra.lib.literals.readUint16.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT16-PARSES-VALID-LITERALS] (requirement)** parseUint16 returns the uint16 parsed from a valid non-negative integer literal within range, wrapped.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT16-NONE-ON-INVALID-OR-NEGATIVE-INPUT] (requirement)** parseUint16 returns none when the string is not a valid integer literal, or when it represents a negative value.

## parseUint32

Parse a string as a uint32.

`parseUint32(s: string) → optional<uint32>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<uint32>`

*Since: 0.17.6.*

parseUint32(s) returns Just(x) where x is the uint32 parsed from s, or Nothing if s is not a valid non-negative integer literal or the parsed value is outside [0, 2^32-1].

Total.

Renamed from hydra.lib.literals.readUint32.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT32-PARSES-VALID-LITERALS] (requirement)** parseUint32 returns the uint32 parsed from a valid non-negative integer literal within range, wrapped, including at the maximum representable value (2^32-1).

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT32-NONE-ON-INVALID-OR-NEGATIVE-INPUT] (requirement)** parseUint32 returns none when the string is not a valid integer literal, or when it represents a negative value.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT32-NONE-ON-OVERFLOW] (requirement)** parseUint32 returns none when the parsed value exceeds the uint32 maximum (2^32-1), one past the boundary.

## parseUint64

Parse a string as a uint64.

`parseUint64(s: string) → optional<uint64>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<uint64>`

*Since: 0.17.6.*

parseUint64(s) returns Just(x) where x is the uint64 parsed from s, or Nothing if s is not a valid non-negative integer literal or the parsed value is outside [0, 2^64-1].

Total.

Renamed from hydra.lib.literals.readUint64.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT64-PARSES-VALID-LITERALS] (requirement)** parseUint64 returns the uint64 parsed from a valid non-negative integer literal within range, wrapped, including large in-range values.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT64-NONE-ON-INVALID-OR-NEGATIVE-INPUT] (requirement)** parseUint64 returns none when the string is not a valid integer literal, or when it represents a negative value.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT64-NONE-ON-OVERFLOW] (requirement)** parseUint64 returns none when the parsed value exceeds the uint64 maximum (2^64-1), one past the boundary.

## parseUint8

Parse a string as a uint8.

`parseUint8(s: string) → optional<uint8>`

**Parameters**

- `s` (`string`): the string to parse

**Returns** `optional<uint8>`

*Since: 0.17.6.*

parseUint8(s) returns Just(x) where x is the uint8 parsed from s, or Nothing if s is not a valid non-negative integer literal or the parsed value is outside [0, 255].

Total.

Renamed from hydra.lib.literals.readUint8.

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT8-PARSES-VALID-LITERALS] (requirement)** parseUint8 returns the uint8 parsed from a valid non-negative integer literal within range, wrapped, including at the maximum representable value (255).

**[HYDRA-CORE-LIB-LITERALS-PARSE-UINT8-NONE-ON-INVALID-OR-NEGATIVE-INPUT] (requirement)** parseUint8 returns none when the string is not a valid integer literal, or when it represents a negative value.

## printBigint

Render a bigint as a string.

`printBigint(x: bigint) → string`

**Parameters**

- `x` (`bigint`): the bigint to render

**Returns** `string`

*Since: 0.17.6.*

printBigint(x) returns the canonical decimal representation of x: an optional leading minus sign followed by decimal digits. No leading zeros are produced (except for the value 0 itself, which renders as "0").

Total. The inverse of parseBigint.

Renamed from hydra.lib.literals.showBigint.

**[HYDRA-CORE-LIB-LITERALS-PRINT-BIGINT-RENDERS-CANONICAL-DECIMAL] (requirement)** printBigint renders a bigint as its canonical decimal string, with a leading minus sign for negative values and no leading sign for positive values or zero.

## printBoolean

Render a boolean as a string.

`printBoolean(b: boolean) → string`

**Parameters**

- `b` (`boolean`): the boolean to render

**Returns** `string`

*Since: 0.17.6.*

printBoolean(true) = "true"; printBoolean(false) = "false".

Total. The inverse of parseBoolean.

Renamed from hydra.lib.literals.showBoolean.

**[HYDRA-CORE-LIB-LITERALS-PRINT-BOOLEAN-RENDERS-LOWERCASE-LITERALS] (requirement)** printBoolean renders true as the exact string "true" and false as "false".

## printDecimal

Render a decimal as a string.

`printDecimal(x: decimal) → string`

**Parameters**

- `x` (`decimal`): the decimal to render

**Returns** `string`

*Since: 0.17.6.*

printDecimal(x) returns the canonical decimal representation of x as a string. The exact form (use of scientific notation, trailing zeros, etc.) is host-defined but always round-trips with parseDecimal.

Total.

Renamed from hydra.lib.literals.showDecimal.

**[HYDRA-CORE-LIB-LITERALS-PRINT-DECIMAL-RENDERS-CANONICAL-DECIMAL] (requirement)** printDecimal renders a decimal as a string, for positive and negative whole and fractional values, that always round-trips through parseDecimal back to the original value.

## printFloat32

Render a float32 as a string.

`printFloat32(x: float32) → string`

**Parameters**

- `x` (`float32`): the float32 to render

**Returns** `string`

*Since: 0.17.6.*

printFloat32(x) returns a string representation of x. Finite values use the shortest decimal that round-trips through parseFloat32 back to x. Special values render as "NaN", "Infinity", or "-Infinity" (capitalization is host-defined).

Total.

Renamed from hydra.lib.literals.showFloat32.

**[HYDRA-CORE-LIB-LITERALS-PRINT-FLOAT32-RENDERS-ROUND-TRIPPABLE-REPRESENTATION] (requirement)** printFloat32 renders a float32 as a string, for positive, negative, zero, and small-magnitude values, that round-trips through parseFloat32 back to the original value.

## printFloat64

Render a float64 as a string.

`printFloat64(x: float64) → string`

**Parameters**

- `x` (`float64`): the float64 to render

**Returns** `string`

*Since: 0.17.6.*

printFloat64(x) returns a string representation of x. Finite values use the shortest decimal that round-trips through parseFloat64 back to x. Special values render as "NaN", "Infinity", or "-Infinity" (capitalization is host-defined).

Total.

Renamed from hydra.lib.literals.showFloat64.

**[HYDRA-CORE-LIB-LITERALS-PRINT-FLOAT64-RENDERS-ROUND-TRIPPABLE-REPRESENTATION] (requirement)** printFloat64 renders a float64 as a string, for positive, zero, and small-magnitude values, that round-trips through parseFloat64 back to the original value.

## printInt16

Render an int16 as a string.

`printInt16(x: int16) → string`

**Parameters**

- `x` (`int16`): the int16 to render

**Returns** `string`

*Since: 0.17.6.*

printInt16(x) returns the canonical decimal representation of x: an optional leading minus sign followed by decimal digits.

Total. The inverse of parseInt16.

Renamed from hydra.lib.literals.showInt16.

**[HYDRA-CORE-LIB-LITERALS-PRINT-INT16-RENDERS-CANONICAL-DECIMAL] (requirement)** printInt16 renders an int16 as its canonical decimal string, with a leading minus sign for negative values.

## printInt32

Render an int32 as a string.

`printInt32(x: int32) → string`

**Parameters**

- `x` (`int32`): the int32 to render

**Returns** `string`

*Since: 0.17.6.*

printInt32(x) returns the canonical decimal representation of x: an optional leading minus sign followed by decimal digits.

Total. The inverse of parseInt32.

Renamed from hydra.lib.literals.showInt32.

**[HYDRA-CORE-LIB-LITERALS-PRINT-INT32-RENDERS-CANONICAL-DECIMAL] (requirement)** printInt32 renders an int32 as its canonical decimal string, with a leading minus sign for negative values and no sign for zero.

## printInt64

Render an int64 as a string.

`printInt64(x: int64) → string`

**Parameters**

- `x` (`int64`): the int64 to render

**Returns** `string`

*Since: 0.17.6.*

printInt64(x) returns the canonical decimal representation of x: an optional leading minus sign followed by decimal digits.

Total. The inverse of parseInt64.

Renamed from hydra.lib.literals.showInt64.

**[HYDRA-CORE-LIB-LITERALS-PRINT-INT64-RENDERS-CANONICAL-DECIMAL] (requirement)** printInt64 renders an int64 as its canonical decimal string, with a leading minus sign for negative values.

## printInt8

Render an int8 as a string.

`printInt8(x: int8) → string`

**Parameters**

- `x` (`int8`): the int8 to render

**Returns** `string`

*Since: 0.17.6.*

printInt8(x) returns the canonical decimal representation of x: an optional leading minus sign followed by decimal digits.

Total. The inverse of parseInt8.

Renamed from hydra.lib.literals.showInt8.

**[HYDRA-CORE-LIB-LITERALS-PRINT-INT8-RENDERS-CANONICAL-DECIMAL] (requirement)** printInt8 renders an int8 as its canonical decimal string, with a leading minus sign for negative values.

## printString

Render a string as a string-literal token (escaped and quoted).

`printString(s: string) → string`

**Parameters**

- `s` (`string`): the string to render

**Returns** `string`

*Since: 0.17.6.*

printString(s) returns a Haskell-syntax string literal token representing s: an opening double-quote, the characters of s with special characters escaped (backslash sequences for control characters, quotes, and non-printable Unicode code points), and a closing double-quote.

Total. The inverse of parseString.

Renamed from hydra.lib.literals.showString.

**[HYDRA-CORE-LIB-LITERALS-PRINT-STRING-QUOTES-AND-PASSES-THROUGH-PLAIN-CHARACTERS] (requirement)** printString wraps a string in double quotes, passing plain ASCII characters through unescaped, for the empty string and ordinary text.

**[HYDRA-CORE-LIB-LITERALS-PRINT-STRING-ESCAPES-NON-ASCII-AS-DECIMAL-CODES] (requirement)** printString renders every non-ASCII character as a backslash followed by its decimal Unicode code point, whether the character appears alone, mixed with ASCII text, or repeated.

**[HYDRA-CORE-LIB-LITERALS-PRINT-STRING-ESCAPES-NAMED-CONTROL-CHARACTERS] (requirement)** printString renders the characters with standard Haskell-syntax named escapes (tab, newline, carriage return, backslash, double quote, NUL, bell, backspace, form feed, vertical tab, delete) using their named escape form, not a raw or decimal-code representation.

## printUint16

Render a uint16 as a string.

`printUint16(x: uint16) → string`

**Parameters**

- `x` (`uint16`): the uint16 to render

**Returns** `string`

*Since: 0.17.6.*

printUint16(x) returns the canonical decimal representation of x.

Total. The inverse of parseUint16.

Renamed from hydra.lib.literals.showUint16.

**[HYDRA-CORE-LIB-LITERALS-PRINT-UINT16-RENDERS-CANONICAL-DECIMAL] (requirement)** printUint16 renders a uint16 as its canonical unsigned decimal string, for zero and typical values.

## printUint32

Render a uint32 as a string.

`printUint32(x: uint32) → string`

**Parameters**

- `x` (`uint32`): the uint32 to render

**Returns** `string`

*Since: 0.17.6.*

printUint32(x) returns the canonical decimal representation of x.

Total. The inverse of parseUint32.

Renamed from hydra.lib.literals.showUint32.

**[HYDRA-CORE-LIB-LITERALS-PRINT-UINT32-RENDERS-CANONICAL-DECIMAL] (requirement)** printUint32 renders a uint32 as its canonical unsigned decimal string, for zero and typical values.

## printUint64

Render a uint64 as a string.

`printUint64(x: uint64) → string`

**Parameters**

- `x` (`uint64`): the uint64 to render

**Returns** `string`

*Since: 0.17.6.*

printUint64(x) returns the canonical decimal representation of x.

Total. The inverse of parseUint64.

Renamed from hydra.lib.literals.showUint64.

**[HYDRA-CORE-LIB-LITERALS-PRINT-UINT64-RENDERS-CANONICAL-DECIMAL] (requirement)** printUint64 renders a uint64 as its canonical unsigned decimal string, for zero and typical values.

## printUint8

Render a uint8 as a string.

`printUint8(x: uint8) → string`

**Parameters**

- `x` (`uint8`): the uint8 to render

**Returns** `string`

*Since: 0.17.6.*

printUint8(x) returns the canonical decimal representation of x.

Total. The inverse of parseUint8.

Renamed from hydra.lib.literals.showUint8.

**[HYDRA-CORE-LIB-LITERALS-PRINT-UINT8-RENDERS-CANONICAL-DECIMAL] (requirement)** printUint8 renders a uint8 as its canonical unsigned decimal string, for zero and the maximum representable value (255).

## uint16ToBigint

Convert a uint16 to a bigint.

`uint16ToBigint(x: uint16) → bigint`

**Parameters**

- `x` (`uint16`): the uint16 to convert

**Returns** `bigint`

*Since: 0.15.*

uint16ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-UINT16-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** uint16ToBigint returns the bigint with the same numeric value as the uint16, with no precision loss, for zero and typical in-range values.

## uint32ToBigint

Convert a uint32 to a bigint.

`uint32ToBigint(x: uint32) → bigint`

**Parameters**

- `x` (`uint32`): the uint32 to convert

**Returns** `bigint`

*Since: 0.15.*

uint32ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-UINT32-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** uint32ToBigint returns the bigint with the same numeric value as the uint32, with no precision loss, for zero and typical in-range values.

## uint64ToBigint

Convert a uint64 to a bigint.

`uint64ToBigint(x: uint64) → bigint`

**Parameters**

- `x` (`uint64`): the uint64 to convert

**Returns** `bigint`

*Since: 0.15.*

uint64ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-UINT64-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** uint64ToBigint returns the bigint with the same numeric value as the uint64, with no precision loss, for zero and typical in-range values.

## uint8ToBigint

Convert a uint8 to a bigint.

`uint8ToBigint(x: uint8) → bigint`

**Parameters**

- `x` (`uint8`): the uint8 to convert

**Returns** `bigint`

*Since: 0.15.*

uint8ToBigint(x) returns the bigint with the same numeric value as x. The conversion is exact and lossless.

Total.

**[HYDRA-CORE-LIB-LITERALS-UINT8-TO-BIGINT-LOSSLESS-CONVERSION] (requirement)** uint8ToBigint returns the bigint with the same numeric value as the uint8, with no precision loss, for zero and the maximum representable value (255).
