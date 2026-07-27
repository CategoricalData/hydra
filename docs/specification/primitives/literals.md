<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Literals.hs (generator not
     yet built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.literals

Conversion and parse/print for literal values.
The numeric conversion primitives (`bigintToInt32`, `float64ToFloat32`, ...) convert between
Hydra's literal numeric types; lossless conversions are exact, and each lossy conversion states
its truncation or rounding behavior in its entry.
The `parse*` primitives are pure conversions from text to values, with an `optional` codomain
wherever parsing can fail; the `print*` primitives are the pure conversions back from values to
text (see [Verb categories](index.md#verb-categories)).
The per-type `parseX`/`printX` family may later collapse into constraint-polymorphic `parse` and
`print` primitives, pending type-class machinery. <!-- [PENDING #566] -->

#### bigintToDecimal — **Draft**

`bigint → decimal`

Usage: `bigintToDecimal x`

Convert a bigint to a decimal.
Returns the decimal with numeric value `x` and scale 0.
Both `bigint` and `decimal` are arbitrary-precision types, so the conversion is lossless.

Since: 0.15

#### bigintToInt16 — **Draft**

`bigint → int16`

Usage: `bigintToInt16 x`

Convert a bigint to an int16, truncating.
Returns `x` reduced modulo 2^16 and reinterpreted as a signed two's-complement int16.
Values outside [-2^15, 2^15-1] wrap silently; no exception is raised.

Since: 0.15

#### bigintToInt32 — **Draft**

`bigint → int32`

Usage: `bigintToInt32 x`

Convert a bigint to an int32, truncating.
Returns `x` reduced modulo 2^32 and reinterpreted as a signed two's-complement int32.
Values outside [-2^31, 2^31-1] wrap silently; no exception is raised.

Since: 0.15

#### bigintToInt64 — **Draft**

`bigint → int64`

Usage: `bigintToInt64 x`

Convert a bigint to an int64, truncating.
Returns `x` reduced modulo 2^64 and reinterpreted as a signed two's-complement int64.
Values outside [-2^63, 2^63-1] wrap silently; no exception is raised.

Since: 0.15

#### bigintToInt8 — **Draft**

`bigint → int8`

Usage: `bigintToInt8 x`

Convert a bigint to an int8, truncating.
Returns `x` reduced modulo 2^8 and reinterpreted as a signed two's-complement int8.
Values outside [-128, 127] wrap silently; no exception is raised.

Since: 0.15

#### bigintToUint16 — **Draft**

`bigint → uint16`

Usage: `bigintToUint16 x`

Convert a bigint to a uint16, truncating.
Returns `x` reduced modulo 2^16 as an unsigned uint16.
Values outside [0, 2^16-1] wrap silently; no exception is raised.

Since: 0.15

#### bigintToUint32 — **Draft**

`bigint → uint32`

Usage: `bigintToUint32 x`

Convert a bigint to a uint32, truncating.
Returns `x` reduced modulo 2^32 as an unsigned uint32.
Values outside [0, 2^32-1] wrap silently; no exception is raised.

Since: 0.15

#### bigintToUint64 — **Draft**

`bigint → uint64`

Usage: `bigintToUint64 x`

Convert a bigint to a uint64, truncating.
Returns `x` reduced modulo 2^64 as an unsigned uint64.
Values outside [0, 2^64-1] wrap silently; no exception is raised.

Since: 0.15

#### bigintToUint8 — **Draft**

`bigint → uint8`

Usage: `bigintToUint8 x`

Convert a bigint to a uint8, truncating.
Returns `x` reduced modulo 2^8 as an unsigned uint8.
Values outside [0, 255] wrap silently; no exception is raised.

Since: 0.15

#### binaryToBytes — **Draft**

`binary → list<uint8>`

Usage: `binaryToBytes b`

Convert binary data to a list of byte values.
Returns the bytes of `b` as a uint8 list, preserving byte order.
Bytes are unsigned 8-bit integers, so the element type is `uint8`: every byte value is
representable, and no out-of-range case exists.
The inverse of `bytesToBinary`: `bytesToBinary (binaryToBytes b)` is `b` for every binary
value `b`.

Since: 0.15

#### bytesToBinary — **Draft**

`list<uint8> → binary`

Usage: `bytesToBinary bytes`

Convert a list of byte values to binary data.
Returns the binary value whose bytes are the elements of `bytes`, in order.
Every `uint8` value is a valid byte, so the conversion is total with no out-of-range case.
The inverse of `binaryToBytes`.

Since: 0.18

#### decimalToBigint — **Draft**

`decimal → bigint`

Usage: `decimalToBigint x`

Convert a decimal to a bigint, truncating.
Returns `x` truncated toward zero; any fractional part is discarded.

Since: 0.15

#### decimalToFloat32 — **Draft**

`decimal → float32`

Usage: `decimalToFloat32 x`

Convert a decimal to a float32.
Returns the IEEE 754 binary32 value closest to `x` under roundTiesToEven.
Values outside the float32 representable range overflow to ±∞; subnormal precision loss is
silent.

Since: 0.15

#### decimalToFloat64 — **Draft**

`decimal → float64`

Usage: `decimalToFloat64 x`

Convert a decimal to a float64.
Returns the IEEE 754 binary64 value closest to `x` under roundTiesToEven.
Values outside the float64 representable range overflow to ±∞; subnormal precision loss is
silent.

Since: 0.15

#### float32ToDecimal — **Draft**

`float32 → optional<decimal>`

Usage: `float32ToDecimal x`

Convert a float32 to a decimal, if finite.
Finite values convert exactly: every finite binary32 value is a dyadic rational, so the result
is `given` the exact decimal representation of `x`, at minimal scale (no trailing fractional
zeros).
Non-finite arguments (±∞, NaN) yield `none`: `decimal` has no non-finite values, so the
partiality is expressed in the codomain.

Since: 0.15

#### float32ToFloat64 — **Draft**

`float32 → float64`

Usage: `float32ToFloat64 x`

Convert a float32 to a float64.
Returns the binary64 value with the same numeric value as `x`; ±∞ and NaN pass through.
The conversion is exact and lossless.

Since: 0.15

#### float64ToDecimal — **Draft**

`float64 → optional<decimal>`

Usage: `float64ToDecimal x`

Convert a float64 to a decimal, if finite.
Finite values convert exactly: every finite binary64 value is a dyadic rational, so the result
is `given` the exact decimal representation of `x`, at minimal scale (no trailing fractional
zeros).
Non-finite arguments (±∞, NaN) yield `none`: `decimal` has no non-finite values, so the
partiality is expressed in the codomain.

Since: 0.15

#### float64ToFloat32 — **Draft**

`float64 → float32`

Usage: `float64ToFloat32 x`

Convert a float64 to a float32 (lossy).
Returns the binary32 value closest to `x` under roundTiesToEven.
Values outside the binary32 range overflow to ±∞; subnormal precision loss is silent; NaN
converts to NaN (Hydra does not distinguish NaN payloads — see
[Floating-point values](index.md#floating-point-values)).

Since: 0.15

#### int16ToBigint — **Draft**

`int16 → bigint`

Usage: `int16ToBigint x`

Convert an int16 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### int32ToBigint — **Draft**

`int32 → bigint`

Usage: `int32ToBigint x`

Convert an int32 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### int64ToBigint — **Draft**

`int64 → bigint`

Usage: `int64ToBigint x`

Convert an int64 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### int8ToBigint — **Draft**

`int8 → bigint`

Usage: `int8ToBigint x`

Convert an int8 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### parseBigint — **Draft**

`string → optional<bigint>`

Usage: `parseBigint s`

Parse a string as a bigint.
Returns `given x` where `x` is the bigint denoted by `s`, or `none` if `s` is not a valid
bigint literal.
The accepted syntax is an optional leading minus sign followed by one or more decimal digits.
Whitespace is not stripped; leading or trailing whitespace causes parse failure.
The inverse of `printBigint`.

Since: 0.18 (renamed from `hydra.lib.literals.readBigint`)

#### parseBoolean — **Draft**

`string → optional<boolean>`

Usage: `parseBoolean s`

Parse a string as a boolean.
Returns `given true` for the input `"true"`, `given false` for `"false"`, and `none` for any
other input.
Exactly the lowercase forms `"true"` and `"false"` are accepted; every other input —
including capitalized or whitespace-padded variants — yields `none`.
The inverse of `printBoolean`.

Since: 0.18 (renamed from `hydra.lib.literals.readBoolean`)

#### parseDecimal — **Draft**

`string → optional<decimal>`

Usage: `parseDecimal s`

Parse a string as a decimal.
Returns `given x` where `x` is the decimal denoted by `s`, or `none` on parse failure.
The accepted syntax is exactly the JSON (RFC 8259) number grammar: an optional minus sign, an
integer part, an optional fractional part, and an optional exponent in either letter case
with an optional sign (e.g. `-1.5`, `2e10`, `6.02E-23`, `0.0`).
Parsing preserves scale: the digit string determines the coefficient and scale of the result
exactly, so `"1.10"` and `"1.1"` parse to distinct decimal values (see
[ordering-and-equality.md](../ordering-and-equality.md)).
Input that is not a single complete number token yields `none`.
The inverse of `printDecimal`.

Since: 0.18 (renamed from `hydra.lib.literals.readDecimal`)

#### parseFloat32 — **Draft**

`string → optional<float32>`

Usage: `parseFloat32 s`

Parse a string as a float32.
Returns `given x` where `x` is the IEEE 754 binary32 value closest to the number denoted by
`s` under roundTiesToEven, or `none` on parse failure.
The special values are accepted under exactly the spellings `"NaN"`, `"Infinity"`, and
`"-Infinity"`, and negative zero as `"-0.0"` — the JSON wire-format sentinel set.
The inverse of `printFloat32`.

Since: 0.18 (renamed from `hydra.lib.literals.readFloat32`)

#### parseFloat64 — **Draft**

`string → optional<float64>`

Usage: `parseFloat64 s`

Parse a string as a float64.
Returns `given x` where `x` is the IEEE 754 binary64 value closest to the number denoted by
`s` under roundTiesToEven, or `none` on parse failure.
The special values are accepted under exactly the spellings `"NaN"`, `"Infinity"`, and
`"-Infinity"`, and negative zero as `"-0.0"` — the JSON wire-format sentinel set.
The inverse of `printFloat64`.

Since: 0.18 (renamed from `hydra.lib.literals.readFloat64`)

#### parseInt16 — **Draft**

`string → optional<int16>`

Usage: `parseInt16 s`

Parse a string as an int16.
Returns `given x` where `x` is the int16 denoted by `s`, or `none` if `s` is not a valid
integer literal or the denoted value is outside [-2^15, 2^15-1].
The accepted syntax is that of `parseBigint`: an optional leading minus sign followed by
decimal digits, with no surrounding whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printInt16`.

Since: 0.18 (renamed from `hydra.lib.literals.readInt16`)

#### parseInt32 — **Draft**

`string → optional<int32>`

Usage: `parseInt32 s`

Parse a string as an int32.
Returns `given x` where `x` is the int32 denoted by `s`, or `none` if `s` is not a valid
integer literal or the denoted value is outside [-2^31, 2^31-1].
The accepted syntax is that of `parseBigint`: an optional leading minus sign followed by
decimal digits, with no surrounding whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printInt32`.

Since: 0.18 (renamed from `hydra.lib.literals.readInt32`)

#### parseInt64 — **Draft**

`string → optional<int64>`

Usage: `parseInt64 s`

Parse a string as an int64.
Returns `given x` where `x` is the int64 denoted by `s`, or `none` if `s` is not a valid
integer literal or the denoted value is outside [-2^63, 2^63-1].
The accepted syntax is that of `parseBigint`: an optional leading minus sign followed by
decimal digits, with no surrounding whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printInt64`.

Since: 0.18 (renamed from `hydra.lib.literals.readInt64`)

#### parseInt8 — **Draft**

`string → optional<int8>`

Usage: `parseInt8 s`

Parse a string as an int8.
Returns `given x` where `x` is the int8 denoted by `s`, or `none` if `s` is not a valid
integer literal or the denoted value is outside [-128, 127].
The accepted syntax is that of `parseBigint`: an optional leading minus sign followed by
decimal digits, with no surrounding whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printInt8`.

Since: 0.18 (renamed from `hydra.lib.literals.readInt8`)

#### parseString — **Draft**

`string → optional<string>`

Usage: `parseString s`

Parse a quoted string-literal token to the string it denotes.
Parses `s` as a string-literal token — an opening double-quote, a body in which special
characters are represented by backslash escape sequences, and a closing double-quote — and
returns `given` the decoded payload on success, or `none` on malformed input.
The accepted grammar is exactly the JSON (RFC 8259) string grammar: `\uXXXX` escapes accept
either hex case, and a surrogate pair of escapes denotes the corresponding supplementary code
point.
Input that is not a single complete JSON string token yields `none` — including unbalanced
quotes, trailing characters after the closing quote, invalid escape sequences, and unpaired
surrogate escapes (which denote no Unicode string, since Hydra strings are sequences of
Unicode scalar values).
The inverse of `printString`.

Since: 0.18 (renamed from `hydra.lib.literals.readString`)

#### parseUint16 — **Draft**

`string → optional<uint16>`

Usage: `parseUint16 s`

Parse a string as a uint16.
Returns `given x` where `x` is the uint16 denoted by `s`, or `none` if `s` is not a valid
non-negative integer literal or the denoted value is outside [0, 2^16-1].
The accepted syntax is a sequence of decimal digits, with no sign and no surrounding
whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printUint16`.

Since: 0.18 (renamed from `hydra.lib.literals.readUint16`)

#### parseUint32 — **Draft**

`string → optional<uint32>`

Usage: `parseUint32 s`

Parse a string as a uint32.
Returns `given x` where `x` is the uint32 denoted by `s`, or `none` if `s` is not a valid
non-negative integer literal or the denoted value is outside [0, 2^32-1].
The accepted syntax is a sequence of decimal digits, with no sign and no surrounding
whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printUint32`.

Since: 0.18 (renamed from `hydra.lib.literals.readUint32`)

#### parseUint64 — **Draft**

`string → optional<uint64>`

Usage: `parseUint64 s`

Parse a string as a uint64.
Returns `given x` where `x` is the uint64 denoted by `s`, or `none` if `s` is not a valid
non-negative integer literal or the denoted value is outside [0, 2^64-1].
The accepted syntax is a sequence of decimal digits, with no sign and no surrounding
whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printUint64`.

Since: 0.18 (renamed from `hydra.lib.literals.readUint64`)

#### parseUint8 — **Draft**

`string → optional<uint8>`

Usage: `parseUint8 s`

Parse a string as a uint8.
Returns `given x` where `x` is the uint8 denoted by `s`, or `none` if `s` is not a valid
non-negative integer literal or the denoted value is outside [0, 255].
The accepted syntax is a sequence of decimal digits, with no sign and no surrounding
whitespace.
Out-of-range values cause parse failure; they do not wrap.
The inverse of `printUint8`.

Since: 0.18 (renamed from `hydra.lib.literals.readUint8`)

#### printBigint — **Draft**

`bigint → string`

Usage: `printBigint x`

Render a bigint as a string.
Returns the canonical decimal representation of `x`: an optional leading minus sign followed
by decimal digits, with no leading zeros (the value 0 renders as `"0"`).
`parseBigint (printBigint x)` is `given x` for every bigint `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showBigint`)

#### printBoolean — **Draft**

`boolean → string`

Usage: `printBoolean x`

Render a boolean as a string.
`printBoolean true` is `"true"`, and `printBoolean false` is `"false"`.
`parseBoolean (printBoolean x)` is `given x` for both boolean values.

Since: 0.18 (renamed from `hydra.lib.literals.showBoolean`)

#### printDecimal — **Draft**

`decimal → string`

Usage: `printDecimal x`

Render a decimal in its canonical form.
The form is representation-faithful: a decimal value is an integer coefficient with a scale
(see [ordering-and-equality.md](../ordering-and-equality.md)), and the printed digit
string preserves the coefficient's digits exactly — trailing zeros included — so distinct
values print distinctly: `1.10` and `1.1` are different decimals and produce different
strings.
The layout follows the ECMAScript `Number::toString` thresholds and spellings: with `a` the
adjusted exponent (the decimal position of the leading significant digit), positional
notation is used when `−7 ≤ a < 21` (no leading zeros, no added or removed trailing digits,
no decimal point for scale-0 values), and exponent form otherwise — one digit before the
point, the remaining coefficient digits after it, a lowercase `e`, and an always-signed
exponent, as in `1e+30`, `1.10e+30`, and `6.02e-23`.
Zero prints as `0`, `0.0`, `0.00`, … according to its scale.
Round-trip law: `parseDecimal (printDecimal x)` is `given x` for every decimal `x`.
The JSON coder renders and reads number payloads via `printDecimal`/`parseDecimal` directly;
this canonical form is therefore also the JSON wire form of a decimal, and scale survives
the wire.

Since: 0.18 (renamed from `hydra.lib.literals.showDecimal`)

#### printFloat32 — **Draft**

`float32 → string`

Usage: `printFloat32 x`

Render a float32 as a string.
Finite values use the shortest decimal representation that round-trips:
`parseFloat32 (printFloat32 x)` is `given x` for every float32 `x`.
The special values print under exactly the spellings `"NaN"`, `"Infinity"`, and
`"-Infinity"`, and negative zero as `"-0.0"` — the JSON wire-format sentinel set.

Since: 0.18 (renamed from `hydra.lib.literals.showFloat32`)

#### printFloat64 — **Draft**

`float64 → string`

Usage: `printFloat64 x`

Render a float64 as a string.
Finite values use the shortest decimal representation that round-trips:
`parseFloat64 (printFloat64 x)` is `given x` for every float64 `x`.
The special values print under exactly the spellings `"NaN"`, `"Infinity"`, and
`"-Infinity"`, and negative zero as `"-0.0"` — the JSON wire-format sentinel set.

Since: 0.18 (renamed from `hydra.lib.literals.showFloat64`)

#### printInt16 — **Draft**

`int16 → string`

Usage: `printInt16 x`

Render an int16 as a string.
Returns the canonical decimal representation of `x`: an optional leading minus sign followed
by decimal digits, with no leading zeros.
`parseInt16 (printInt16 x)` is `given x` for every int16 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showInt16`)

#### printInt32 — **Draft**

`int32 → string`

Usage: `printInt32 x`

Render an int32 as a string.
Returns the canonical decimal representation of `x`: an optional leading minus sign followed
by decimal digits, with no leading zeros.
`parseInt32 (printInt32 x)` is `given x` for every int32 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showInt32`)

#### printInt64 — **Draft**

`int64 → string`

Usage: `printInt64 x`

Render an int64 as a string.
Returns the canonical decimal representation of `x`: an optional leading minus sign followed
by decimal digits, with no leading zeros.
`parseInt64 (printInt64 x)` is `given x` for every int64 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showInt64`)

#### printInt8 — **Draft**

`int8 → string`

Usage: `printInt8 x`

Render an int8 as a string.
Returns the canonical decimal representation of `x`: an optional leading minus sign followed
by decimal digits, with no leading zeros.
`parseInt8 (printInt8 x)` is `given x` for every int8 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showInt8`)

#### printString — **Draft**

`string → string`

Usage: `printString x`

Render a string as a quoted string-literal token.
Returns a token consisting of an opening double-quote, the characters of `x` with special
characters (quotes, backslashes, control characters) represented by backslash escape
sequences, and a closing double-quote.
The produced form is the RFC 8785 (JCS) canonical JSON string serialization: only `"`, `\`,
and the control characters U+0000–U+001F are escaped; the short escapes (`\b`, `\t`, `\n`,
`\f`, `\r`) are used where they exist and `\u00xx` with lowercase hex otherwise; all other
characters — including non-ASCII — appear unescaped as raw text.
`parseString (printString x)` is `given x` for every string `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showString`)

#### printUint16 — **Draft**

`uint16 → string`

Usage: `printUint16 x`

Render a uint16 as a string.
Returns the canonical decimal representation of `x`: decimal digits with no sign and no
leading zeros.
`parseUint16 (printUint16 x)` is `given x` for every uint16 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showUint16`)

#### printUint32 — **Draft**

`uint32 → string`

Usage: `printUint32 x`

Render a uint32 as a string.
Returns the canonical decimal representation of `x`: decimal digits with no sign and no
leading zeros.
`parseUint32 (printUint32 x)` is `given x` for every uint32 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showUint32`)

#### printUint64 — **Draft**

`uint64 → string`

Usage: `printUint64 x`

Render a uint64 as a string.
Returns the canonical decimal representation of `x`: decimal digits with no sign and no
leading zeros.
`parseUint64 (printUint64 x)` is `given x` for every uint64 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showUint64`)

#### printUint8 — **Draft**

`uint8 → string`

Usage: `printUint8 x`

Render a uint8 as a string.
Returns the canonical decimal representation of `x`: decimal digits with no sign and no
leading zeros.
`parseUint8 (printUint8 x)` is `given x` for every uint8 `x`.

Since: 0.18 (renamed from `hydra.lib.literals.showUint8`)

#### uint16ToBigint — **Draft**

`uint16 → bigint`

Usage: `uint16ToBigint x`

Convert a uint16 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### uint32ToBigint — **Draft**

`uint32 → bigint`

Usage: `uint32ToBigint x`

Convert a uint32 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### uint64ToBigint — **Draft**

`uint64 → bigint`

Usage: `uint64ToBigint x`

Convert a uint64 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### uint8ToBigint — **Draft**

`uint8 → bigint`

Usage: `uint8ToBigint x`

Convert a uint8 to a bigint.
Returns the bigint with the same numeric value as `x`; the conversion is exact and lossless.

Since: 0.15

#### binaryToString — **Deprecated**

`binary → string`

Deprecated since: 0.18. Use: `hydra.lib.text.decodeUtf8`, which returns an either with an
explicit error instead of host-defined replacement.

#### readBigint — **Deprecated**

`string → optional<bigint>`

Deprecated since: 0.18. Use: `parseBigint`.

#### readBoolean — **Deprecated**

`string → optional<boolean>`

Deprecated since: 0.18. Use: `parseBoolean`.

#### readDecimal — **Deprecated**

`string → optional<decimal>`

Deprecated since: 0.18. Use: `parseDecimal`.

#### readFloat32 — **Deprecated**

`string → optional<float32>`

Deprecated since: 0.18. Use: `parseFloat32`.

#### readFloat64 — **Deprecated**

`string → optional<float64>`

Deprecated since: 0.18. Use: `parseFloat64`.

#### readInt16 — **Deprecated**

`string → optional<int16>`

Deprecated since: 0.18. Use: `parseInt16`.

#### readInt32 — **Deprecated**

`string → optional<int32>`

Deprecated since: 0.18. Use: `parseInt32`.

#### readInt64 — **Deprecated**

`string → optional<int64>`

Deprecated since: 0.18. Use: `parseInt64`.

#### readInt8 — **Deprecated**

`string → optional<int8>`

Deprecated since: 0.18. Use: `parseInt8`.

#### readString — **Deprecated**

`string → optional<string>`

Deprecated since: 0.18. Use: `parseString`.

#### readUint16 — **Deprecated**

`string → optional<uint16>`

Deprecated since: 0.18. Use: `parseUint16`.

#### readUint32 — **Deprecated**

`string → optional<uint32>`

Deprecated since: 0.18. Use: `parseUint32`.

#### readUint64 — **Deprecated**

`string → optional<uint64>`

Deprecated since: 0.18. Use: `parseUint64`.

#### readUint8 — **Deprecated**

`string → optional<uint8>`

Deprecated since: 0.18. Use: `parseUint8`.

#### showBigint — **Deprecated**

`bigint → string`

Deprecated since: 0.18. Use: `printBigint`.

#### showBoolean — **Deprecated**

`boolean → string`

Deprecated since: 0.18. Use: `printBoolean`.

#### showDecimal — **Deprecated**

`decimal → string`

Deprecated since: 0.18. Use: `printDecimal`.

#### showFloat32 — **Deprecated**

`float32 → string`

Deprecated since: 0.18. Use: `printFloat32`.

#### showFloat64 — **Deprecated**

`float64 → string`

Deprecated since: 0.18. Use: `printFloat64`.

#### showInt16 — **Deprecated**

`int16 → string`

Deprecated since: 0.18. Use: `printInt16`.

#### showInt32 — **Deprecated**

`int32 → string`

Deprecated since: 0.18. Use: `printInt32`.

#### showInt64 — **Deprecated**

`int64 → string`

Deprecated since: 0.18. Use: `printInt64`.

#### showInt8 — **Deprecated**

`int8 → string`

Deprecated since: 0.18. Use: `printInt8`.

#### showString — **Deprecated**

`string → string`

Deprecated since: 0.18. Use: `printString`.

#### showUint16 — **Deprecated**

`uint16 → string`

Deprecated since: 0.18. Use: `printUint16`.

#### showUint32 — **Deprecated**

`uint32 → string`

Deprecated since: 0.18. Use: `printUint32`.

#### showUint64 — **Deprecated**

`uint64 → string`

Deprecated since: 0.18. Use: `printUint64`.

#### showUint8 — **Deprecated**

`uint8 → string`

Deprecated since: 0.18. Use: `printUint8`.

#### stringToBinary — **Deprecated**

`string → binary`

Deprecated since: 0.18. Use: `hydra.lib.text.encodeUtf8`, which returns an either with an
explicit error instead of host-defined replacement.
