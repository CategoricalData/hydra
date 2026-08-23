// Package literals provides literal type conversion functions for Hydra.
// This corresponds to hydra.lib.literals in the Haskell implementation.
package literals

import (
	"encoding/base64"
	"math"
	"math/big"
	"strconv"
	"strings"
)

// BigfloatToBigint converts a big float to a big int (truncating toward zero).
// hydra.lib.literals.bigfloatToBigint : Bigfloat -> Bigint
func BigfloatToBigint(f *big.Float) *big.Int {
	i, _ := f.Int(nil)
	return i
}

// BigfloatToFloat32 converts a big float to float32.
// hydra.lib.literals.bigfloatToFloat32 : Bigfloat -> Float32
func BigfloatToFloat32(f *big.Float) float32 {
	f64, _ := f.Float64()
	return float32(f64)
}

// BigfloatToFloat64 converts a big float to float64.
// hydra.lib.literals.bigfloatToFloat64 : Bigfloat -> Float64
func BigfloatToFloat64(f *big.Float) float64 {
	f64, _ := f.Float64()
	return f64
}

// BigintToBigfloat converts a big int to a big float.
// hydra.lib.literals.bigintToBigfloat : Bigint -> Bigfloat
func BigintToBigfloat(i *big.Int) *big.Float {
	return new(big.Float).SetInt(i)
}

// BigintToInt8 converts a big int to int8.
// hydra.lib.literals.bigintToInt8 : Bigint -> Int8
func BigintToInt8(i *big.Int) int8 {
	return int8(i.Int64())
}

// BigintToInt16 converts a big int to int16.
// hydra.lib.literals.bigintToInt16 : Bigint -> Int16
func BigintToInt16(i *big.Int) int16 {
	return int16(i.Int64())
}

// BigintToInt32 converts a big int to int32.
// hydra.lib.literals.bigintToInt32 : Bigint -> Int32
func BigintToInt32(i *big.Int) int32 {
	return int32(i.Int64())
}

// BigintToInt64 converts a big int to int64.
// hydra.lib.literals.bigintToInt64 : Bigint -> Int64
func BigintToInt64(i *big.Int) int64 {
	return i.Int64()
}

// BigintToUint8 converts a big int to uint8.
// hydra.lib.literals.bigintToUint8 : Bigint -> Uint8
func BigintToUint8(i *big.Int) uint8 {
	return uint8(i.Uint64())
}

// BigintToUint16 converts a big int to uint16.
// hydra.lib.literals.bigintToUint16 : Bigint -> Uint16
func BigintToUint16(i *big.Int) uint16 {
	return uint16(i.Uint64())
}

// BigintToUint32 converts a big int to uint32.
// hydra.lib.literals.bigintToUint32 : Bigint -> Uint32
func BigintToUint32(i *big.Int) uint32 {
	return uint32(i.Uint64())
}

// BigintToUint64 converts a big int to uint64.
// hydra.lib.literals.bigintToUint64 : Bigint -> Uint64
func BigintToUint64(i *big.Int) uint64 {
	return i.Uint64()
}

// BinaryToBase64 converts binary data to a base64-encoded string.
// hydra.lib.literals.binaryToBase64 : Binary -> String
func BinaryToBase64(b []byte) string {
	return base64.StdEncoding.EncodeToString(b)
}

// Float32ToBigfloat converts float32 to big float.
// hydra.lib.literals.float32ToBigfloat : Float32 -> Bigfloat
func Float32ToBigfloat(f float32) *big.Float {
	return big.NewFloat(float64(f))
}

// Float64ToBigfloat converts float64 to big float.
// hydra.lib.literals.float64ToBigfloat : Float64 -> Bigfloat
func Float64ToBigfloat(f float64) *big.Float {
	return big.NewFloat(f)
}

// Int8ToBigint converts int8 to big int.
// hydra.lib.literals.int8ToBigint : Int8 -> Bigint
func Int8ToBigint(i int8) *big.Int {
	return big.NewInt(int64(i))
}

// Int16ToBigint converts int16 to big int.
// hydra.lib.literals.int16ToBigint : Int16 -> Bigint
func Int16ToBigint(i int16) *big.Int {
	return big.NewInt(int64(i))
}

// Int32ToBigint converts int32 to big int.
// hydra.lib.literals.int32ToBigint : Int32 -> Bigint
func Int32ToBigint(i int32) *big.Int {
	return big.NewInt(int64(i))
}

// Int64ToBigint converts int64 to big int.
// hydra.lib.literals.int64ToBigint : Int64 -> Bigint
func Int64ToBigint(i int64) *big.Int {
	return big.NewInt(i)
}

// Uint8ToBigint converts uint8 to big int.
// hydra.lib.literals.uint8ToBigint : Uint8 -> Bigint
func Uint8ToBigint(i uint8) *big.Int {
	return new(big.Int).SetUint64(uint64(i))
}

// Uint16ToBigint converts uint16 to big int.
// hydra.lib.literals.uint16ToBigint : Uint16 -> Bigint
func Uint16ToBigint(i uint16) *big.Int {
	return new(big.Int).SetUint64(uint64(i))
}

// Uint32ToBigint converts uint32 to big int.
// hydra.lib.literals.uint32ToBigint : Uint32 -> Bigint
func Uint32ToBigint(i uint32) *big.Int {
	return new(big.Int).SetUint64(uint64(i))
}

// Uint64ToBigint converts uint64 to big int.
// hydra.lib.literals.uint64ToBigint : Uint64 -> Bigint
func Uint64ToBigint(i uint64) *big.Int {
	return new(big.Int).SetUint64(i)
}

// Base64ToBinary converts a base64-encoded string to binary data.
// hydra.lib.literals.base64ToBinary : String -> Binary
func Base64ToBinary(s string) []byte {
	b, err := base64.StdEncoding.DecodeString(s)
	if err != nil {
		return []byte(s) // fallback to raw bytes
	}
	return b
}

// ReadBigfloat parses a string as a big float.
// hydra.lib.literals.readBigfloat : String -> Maybe Bigfloat
func ReadBigfloat(s string) **big.Float {
	f, _, err := big.ParseFloat(strings.TrimSpace(s), 10, 256, big.ToNearestEven)
	if err != nil {
		return nil
	}
	return &f
}

// ParseBigint parses a string as a big int.
// hydra.lib.literals.parseBigint : String -> Maybe Bigint
func ParseBigint(s string) **big.Int {
	i, ok := new(big.Int).SetString(strings.TrimSpace(s), 10)
	if !ok {
		return nil
	}
	return &i
}

// ReadBoolean parses a string as a boolean.
// hydra.lib.literals.readBoolean : String -> Maybe Boolean
func ReadBoolean(s string) *bool {
	s = strings.TrimSpace(strings.ToLower(s))
	switch s {
	case "true":
		b := true
		return &b
	case "false":
		b := false
		return &b
	default:
		return nil
	}
}

// ParseFloat32 parses a string as a float32.
// hydra.lib.literals.parseFloat32 : String -> Maybe Float32
func ParseFloat32(s string) *float32 {
	f, err := strconv.ParseFloat(strings.TrimSpace(s), 32)
	if err != nil {
		return nil
	}
	f32 := float32(f)
	return &f32
}

// ParseFloat64 parses a string as a float64.
// hydra.lib.literals.parseFloat64 : String -> Maybe Float64
func ParseFloat64(s string) *float64 {
	f, err := strconv.ParseFloat(strings.TrimSpace(s), 64)
	if err != nil {
		return nil
	}
	return &f
}

// ParseInt8 parses a string as an int8.
// hydra.lib.literals.parseInt8 : String -> Maybe Int8
func ParseInt8(s string) *int8 {
	i, err := strconv.ParseInt(strings.TrimSpace(s), 10, 8)
	if err != nil {
		return nil
	}
	i8 := int8(i)
	return &i8
}

// ParseInt16 parses a string as an int16.
// hydra.lib.literals.parseInt16 : String -> Maybe Int16
func ParseInt16(s string) *int16 {
	i, err := strconv.ParseInt(strings.TrimSpace(s), 10, 16)
	if err != nil {
		return nil
	}
	i16 := int16(i)
	return &i16
}

// ParseInt32 parses a string as an int32.
// hydra.lib.literals.parseInt32 : String -> Maybe Int32
func ParseInt32(s string) *int32 {
	i, err := strconv.ParseInt(strings.TrimSpace(s), 10, 32)
	if err != nil {
		return nil
	}
	i32 := int32(i)
	return &i32
}

// ParseInt64 parses a string as an int64.
// hydra.lib.literals.parseInt64 : String -> Maybe Int64
func ParseInt64(s string) *int64 {
	i, err := strconv.ParseInt(strings.TrimSpace(s), 10, 64)
	if err != nil {
		return nil
	}
	return &i
}

// ReadString parses a string as a string (identity, always succeeds).
// hydra.lib.literals.readString : String -> Maybe String
func ReadString(s string) *string {
	return &s
}

// ParseUint8 parses a string as a uint8.
// hydra.lib.literals.parseUint8 : String -> Maybe Uint8
func ParseUint8(s string) *uint8 {
	i, err := strconv.ParseUint(strings.TrimSpace(s), 10, 8)
	if err != nil {
		return nil
	}
	u8 := uint8(i)
	return &u8
}

// ParseUint16 parses a string as a uint16.
// hydra.lib.literals.parseUint16 : String -> Maybe Uint16
func ParseUint16(s string) *uint16 {
	i, err := strconv.ParseUint(strings.TrimSpace(s), 10, 16)
	if err != nil {
		return nil
	}
	u16 := uint16(i)
	return &u16
}

// ParseUint32 parses a string as a uint32.
// hydra.lib.literals.parseUint32 : String -> Maybe Uint32
func ParseUint32(s string) *uint32 {
	i, err := strconv.ParseUint(strings.TrimSpace(s), 10, 32)
	if err != nil {
		return nil
	}
	u32 := uint32(i)
	return &u32
}

// ParseUint64 parses a string as a uint64.
// hydra.lib.literals.parseUint64 : String -> Maybe Uint64
func ParseUint64(s string) *uint64 {
	i, err := strconv.ParseUint(strings.TrimSpace(s), 10, 64)
	if err != nil {
		return nil
	}
	return &i
}

// ShowBigfloat converts a big float to its string representation.
// hydra.lib.literals.showBigfloat : Bigfloat -> String
func ShowBigfloat(f *big.Float) string {
	return f.Text('g', -1)
}

// PrintBigint converts a big int to its string representation.
// hydra.lib.literals.printBigint : Bigint -> String
func PrintBigint(i *big.Int) string {
	return i.String()
}

// ShowBoolean converts a boolean to its string representation.
// hydra.lib.literals.showBoolean : Boolean -> String
func ShowBoolean(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

// PrintFloat32 converts a float32 to its string representation.
// hydra.lib.literals.printFloat32 : Float32 -> String
func PrintFloat32(f float32) string {
	return strconv.FormatFloat(float64(f), 'g', -1, 32)
}

// PrintFloat64 converts a float64 to its string representation.
// hydra.lib.literals.printFloat64 : Float64 -> String
func PrintFloat64(f float64) string {
	return strconv.FormatFloat(f, 'g', -1, 64)
}

// PrintInt8 converts an int8 to its string representation.
// hydra.lib.literals.printInt8 : Int8 -> String
func PrintInt8(i int8) string {
	return strconv.FormatInt(int64(i), 10)
}

// PrintInt16 converts an int16 to its string representation.
// hydra.lib.literals.printInt16 : Int16 -> String
func PrintInt16(i int16) string {
	return strconv.FormatInt(int64(i), 10)
}

// PrintInt32 converts an int32 to its string representation.
// hydra.lib.literals.printInt32 : Int32 -> String
func PrintInt32(i int32) string {
	return strconv.FormatInt(int64(i), 10)
}

// PrintInt64 converts an int64 to its string representation.
// hydra.lib.literals.printInt64 : Int64 -> String
func PrintInt64(i int64) string {
	return strconv.FormatInt(i, 10)
}

// PrintUint8 converts a uint8 to its string representation.
// hydra.lib.literals.printUint8 : Uint8 -> String
func PrintUint8(i uint8) string {
	return strconv.FormatUint(uint64(i), 10)
}

// PrintUint16 converts a uint16 to its string representation.
// hydra.lib.literals.printUint16 : Uint16 -> String
func PrintUint16(i uint16) string {
	return strconv.FormatUint(uint64(i), 10)
}

// PrintUint32 converts a uint32 to its string representation.
// hydra.lib.literals.printUint32 : Uint32 -> String
func PrintUint32(i uint32) string {
	return strconv.FormatUint(uint64(i), 10)
}

// PrintUint64 converts a uint64 to its string representation.
// hydra.lib.literals.printUint64 : Uint64 -> String
func PrintUint64(i uint64) string {
	return strconv.FormatUint(i, 10)
}

// ShowString converts a string to its quoted representation.
// hydra.lib.literals.showString : String -> String
func ShowString(s string) string {
	return strconv.Quote(s)
}

// Helper math functions not in standard library

// IsNaN checks if a float64 is NaN.
func IsNaN(f float64) bool {
	return math.IsNaN(f)
}

// IsInf checks if a float64 is infinite.
func IsInf(f float64) bool {
	return math.IsInf(f, 0)
}
