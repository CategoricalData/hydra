// Package literals implements the hydra.lib.literals primitives: conversions
// among Hydra's literal value spaces.
//
// Calling convention (Native Contract v1): every primitive takes and returns
// `any`. Numeric representations: bigint = *big.Int, decimal = *big.Float,
// binary = []byte, the fixed-width ints/uints and float32/64 as the matching Go
// types. Parses (readX) and other partial conversions return util.Optional.
//
// The arbitrary-precision floating type is named "decimal" throughout, matching
// the kernel/lexicon (the earlier Bigfloat* naming was wrong and is gone).
package literals

import (
	"encoding/base64"
	"math/big"
	"strconv"
	"strings"

	"hydra.dev/hydra/overlay/go/util"
)

// ---- bigint -> fixed width ----

// BigintToInt8 : bigint -> int8
func BigintToInt8(i any) any { return int8(i.(*big.Int).Int64()) }

// BigintToInt16 : bigint -> int16
func BigintToInt16(i any) any { return int16(i.(*big.Int).Int64()) }

// BigintToInt32 : bigint -> int32
func BigintToInt32(i any) any { return int32(i.(*big.Int).Int64()) }

// BigintToInt64 : bigint -> int64
func BigintToInt64(i any) any { return i.(*big.Int).Int64() }

// BigintToUint8 : bigint -> uint8
func BigintToUint8(i any) any { return uint8(i.(*big.Int).Uint64()) }

// BigintToUint16 : bigint -> uint16
func BigintToUint16(i any) any { return uint16(i.(*big.Int).Uint64()) }

// BigintToUint32 : bigint -> uint32
func BigintToUint32(i any) any { return uint32(i.(*big.Int).Uint64()) }

// BigintToUint64 : bigint -> uint64
func BigintToUint64(i any) any { return i.(*big.Int).Uint64() }

// BigintToDecimal : bigint -> decimal
func BigintToDecimal(i any) any { return new(big.Float).SetInt(i.(*big.Int)) }

// ---- fixed width -> bigint ----

// Int8ToBigint : int8 -> bigint
func Int8ToBigint(i any) any { return big.NewInt(int64(i.(int8))) }

// Int16ToBigint : int16 -> bigint
func Int16ToBigint(i any) any { return big.NewInt(int64(i.(int16))) }

// BinaryToBase64 : binary -> string (base64-encoded)
func BinaryToBase64(b any) any {
	return base64.StdEncoding.EncodeToString(b.([]byte))
}

// Int32ToBigint : int32 -> bigint
func Int32ToBigint(i any) any { return big.NewInt(int64(i.(int32))) }

// Int64ToBigint : int64 -> bigint
func Int64ToBigint(i any) any { return big.NewInt(i.(int64)) }

// Uint8ToBigint : uint8 -> bigint
func Uint8ToBigint(i any) any { return new(big.Int).SetUint64(uint64(i.(uint8))) }

// Uint16ToBigint : uint16 -> bigint
func Uint16ToBigint(i any) any { return new(big.Int).SetUint64(uint64(i.(uint16))) }

// Uint32ToBigint : uint32 -> bigint
func Uint32ToBigint(i any) any { return new(big.Int).SetUint64(uint64(i.(uint32))) }

// Uint64ToBigint : uint64 -> bigint
func Uint64ToBigint(i any) any { return new(big.Int).SetUint64(i.(uint64)) }

// ---- decimal / float conversions ----

// DecimalToBigint : decimal -> bigint (truncates toward zero)
func DecimalToBigint(f any) any {
	i, _ := f.(*big.Float).Int(nil)
	return i
}

// DecimalToFloat32 : decimal -> float32
func DecimalToFloat32(f any) any {
	v, _ := f.(*big.Float).Float32()
	return v
}

// DecimalToFloat64 : decimal -> float64
func DecimalToFloat64(f any) any {
	v, _ := f.(*big.Float).Float64()
	return v
}

// Float32ToDecimal : float32 -> decimal
func Float32ToDecimal(f any) any { return big.NewFloat(float64(f.(float32))) }

// Float64ToDecimal : float64 -> decimal
func Float64ToDecimal(f any) any { return big.NewFloat(f.(float64)) }

// Float32ToFloat64 : float32 -> float64
func Float32ToFloat64(f any) any { return float64(f.(float32)) }

// Float64ToFloat32 : float64 -> float32
func Float64ToFloat32(f any) any { return float32(f.(float64)) }

// ---- binary <-> bytes/string ----

// BinaryToBytes : binary -> list<int32> (each byte as an int32)
func BinaryToBytes(b any) any {
	bs := b.([]byte)
	out := make([]any, len(bs))
	for i, x := range bs {
		out[i] = int32(x)
	}
	return out
}

// Base64ToBinary : string -> binary
func Base64ToBinary(s any) any {
	b, err := base64.StdEncoding.DecodeString(s.(string))
	if err != nil {
		panic("hydra.lib.literals.base64ToBinary: invalid base64: " + err.Error())
	}
	return b
}

// ---- IntegerValue bridge (bigint <-> the canonical integer value) ----

// BigintToIntegerValue : bigint -> IntegerValue (bigint variant). At the Go
// value level the canonical integer value for a bigint is the bigint itself;
// the coder wraps it into the IntegerValue union at construction sites, so this
// is the identity on *big.Int.
func BigintToIntegerValue(i any) any { return i }

// IntegerValueToBigint : IntegerValue -> bigint. The kernel only feeds the
// bigint-valued integer here; other widths are lifted first.
func IntegerValueToBigint(v any) any { return v }

// ---- parseX : string -> optional<T> ----
//
// The #691 rename replaced the readX family with parseX. These are the whole
// hydra.lib.literals.parse* surface, on the same all-any Native Contract v1 as
// every other primitive here: a failed parse is util.None(), never a nil pointer.

func trimmed(s any) string { return strings.TrimSpace(s.(string)) }

// parseSigned parses a signed integer of the given bit width.
func parseSigned(s any, bits int) (int64, bool) {
	v, err := strconv.ParseInt(trimmed(s), 10, bits)
	return v, err == nil
}

// parseUnsigned parses an unsigned integer of the given bit width.
func parseUnsigned(s any, bits int) (uint64, bool) {
	v, err := strconv.ParseUint(trimmed(s), 10, bits)
	return v, err == nil
}

// ParseBigint : string -> optional<bigint>
func ParseBigint(s any) any {
	if i, ok := new(big.Int).SetString(trimmed(s), 10); ok {
		return util.Some(i)
	}
	return util.None()
}

// ParseBoolean : string -> optional<boolean>
func ParseBoolean(s any) any {
	switch strings.ToLower(trimmed(s)) {
	case "true":
		return util.Some(true)
	case "false":
		return util.Some(false)
	}
	return util.None()
}

// ParseDecimal : string -> optional<decimal>
func ParseDecimal(s any) any {
	if f, ok := new(big.Float).SetString(trimmed(s)); ok {
		return util.Some(f)
	}
	return util.None()
}

// ParseFloat32 : string -> optional<float32>
func ParseFloat32(s any) any {
	f, err := strconv.ParseFloat(trimmed(s), 32)
	if err != nil {
		return util.None()
	}
	return util.Some(float32(f))
}

// ParseFloat64 : string -> optional<float64>
func ParseFloat64(s any) any {
	f, err := strconv.ParseFloat(trimmed(s), 64)
	if err != nil {
		return util.None()
	}
	return util.Some(f)
}

// ParseInt8 : string -> optional<int8>
func ParseInt8(s any) any {
	if v, ok := parseSigned(s, 8); ok {
		return util.Some(int8(v))
	}
	return util.None()
}

// ParseInt16 : string -> optional<int16>
func ParseInt16(s any) any {
	if v, ok := parseSigned(s, 16); ok {
		return util.Some(int16(v))
	}
	return util.None()
}

// ParseInt32 : string -> optional<int32>
func ParseInt32(s any) any {
	if v, ok := parseSigned(s, 32); ok {
		return util.Some(int32(v))
	}
	return util.None()
}

// ParseInt64 : string -> optional<int64>
func ParseInt64(s any) any {
	if v, ok := parseSigned(s, 64); ok {
		return util.Some(v)
	}
	return util.None()
}

// ParseString : string -> optional<string>. Parses a quoted string-literal
// token, decoding backslash escapes; the inverse of PrintString. Malformed
// input (including an unquoted string) yields none.
func ParseString(s any) any {
	if v, err := strconv.Unquote(s.(string)); err == nil {
		return util.Some(v)
	}
	return util.None()
}

// ParseUint8 : string -> optional<uint8>
func ParseUint8(s any) any {
	if v, ok := parseUnsigned(s, 8); ok {
		return util.Some(uint8(v))
	}
	return util.None()
}

// ParseUint16 : string -> optional<uint16>
func ParseUint16(s any) any {
	if v, ok := parseUnsigned(s, 16); ok {
		return util.Some(uint16(v))
	}
	return util.None()
}

// ParseUint32 : string -> optional<uint32>
func ParseUint32(s any) any {
	if v, ok := parseUnsigned(s, 32); ok {
		return util.Some(uint32(v))
	}
	return util.None()
}

// ParseUint64 : string -> optional<uint64>
func ParseUint64(s any) any {
	if v, ok := parseUnsigned(s, 64); ok {
		return util.Some(v)
	}
	return util.None()
}

// ---- printX : T -> string ----

// PrintBigint : bigint -> string
func PrintBigint(i any) any { return i.(*big.Int).String() }

// PrintBoolean : boolean -> string
func PrintBoolean(b any) any {
	if b.(bool) {
		return "true"
	}
	return "false"
}

// PrintDecimal : decimal -> string
func PrintDecimal(f any) any { return f.(*big.Float).Text('g', -1) }

// PrintFloat32 : float32 -> string
func PrintFloat32(f any) any { return strconv.FormatFloat(float64(f.(float32)), 'g', -1, 32) }

// PrintFloat64 : float64 -> string
func PrintFloat64(f any) any { return strconv.FormatFloat(f.(float64), 'g', -1, 64) }

// PrintInt8 : int8 -> string
func PrintInt8(i any) any { return strconv.FormatInt(int64(i.(int8)), 10) }

// PrintInt16 : int16 -> string
func PrintInt16(i any) any { return strconv.FormatInt(int64(i.(int16)), 10) }

// PrintInt32 : int32 -> string
func PrintInt32(i any) any { return strconv.FormatInt(int64(i.(int32)), 10) }

// PrintInt64 : int64 -> string
func PrintInt64(i any) any { return strconv.FormatInt(i.(int64), 10) }

// PrintUint8 : uint8 -> string
func PrintUint8(i any) any { return strconv.FormatUint(uint64(i.(uint8)), 10) }

// PrintUint16 : uint16 -> string
func PrintUint16(i any) any { return strconv.FormatUint(uint64(i.(uint16)), 10) }

// PrintUint32 : uint32 -> string
func PrintUint32(i any) any { return strconv.FormatUint(uint64(i.(uint32)), 10) }

// PrintUint64 : uint64 -> string
func PrintUint64(i any) any { return strconv.FormatUint(i.(uint64), 10) }

// PrintString : string -> string (quoted)
func PrintString(s any) any { return strconv.Quote(s.(string)) }
