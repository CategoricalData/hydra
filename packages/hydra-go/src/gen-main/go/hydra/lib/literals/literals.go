// Note: this is an automatically generated file. Do not edit.

package literals

import (
	"fmt"
	"math/big"
	"strconv"
)

// hydra.lib.literals.showInt : Int -> String
func ShowInt(x any) any {
	return strconv.Itoa(x.(int))
}

// hydra.lib.literals.showUint : Uint -> String
func ShowUint(x any) any {
	return strconv.Itoa(x.(int))
}

// hydra.lib.literals.showString : String -> String
func ShowString(x any) any {
	return x.(string)
}

// hydra.lib.literals.showFloat : Float64 -> String
func ShowFloat(x any) any {
	return strconv.FormatFloat(x.(float64), 'G', -1, 64)
}

// hydra.lib.literals.showBoolean : Boolean -> String
func ShowBoolean(x any) any {
	if x.(bool) {
		return "true"
	}
	return "false"
}

// hydra.lib.literals.bigintToInt : BigInt -> Int
func BigintToInt(x any) any {
	return int(x.(*big.Int).Int64())
}

// hydra.lib.literals.bigintToUint : BigInt -> Uint
func BigintToUint(x any) any {
	return int(x.(*big.Int).Uint64())
}

// hydra.lib.literals.bigintToBigfloat : BigInt -> BigFloat
func BigintToBigfloat(x any) any {
	bf := new(big.Float)
	bf.SetInt(x.(*big.Int))
	return bf
}

// hydra.lib.literals.bigfloatToBigint : BigFloat -> BigInt
func BigfloatToBigint(x any) any {
	bi, _ := x.(*big.Float).Int(nil)
	if bi == nil {
		return new(big.Int)
	}
	return bi
}

// hydra.lib.literals.int32ToBigint : Int32 -> BigInt
func Int32ToBigint(x any) any {
	return big.NewInt(int64(x.(int32)))
}

// hydra.lib.literals.int64ToBigint : Int64 -> BigInt
func Int64ToBigint(x any) any {
	return big.NewInt(x.(int64))
}

// hydra.lib.literals.bigintToInt32 : BigInt -> Int32
func BigintToInt32(x any) any {
	return int32(x.(*big.Int).Int64())
}

// hydra.lib.literals.bigintToInt64 : BigInt -> Int64
func BigintToInt64(x any) any {
	return x.(*big.Int).Int64()
}

// hydra.lib.literals.showInt32 : Int32 -> String
func ShowInt32(x any) any {
	return fmt.Sprintf("%d", x.(int32))
}

// hydra.lib.literals.showInt64 : Int64 -> String
func ShowInt64(x any) any {
	return fmt.Sprintf("%d", x.(int64))
}

// hydra.lib.literals.showFloat32 : Float32 -> String
func ShowFloat32(x any) any {
	return strconv.FormatFloat(float64(x.(float32)), 'G', -1, 32)
}

// hydra.lib.literals.showFloat64 : Float64 -> String
func ShowFloat64(x any) any {
	return strconv.FormatFloat(x.(float64), 'G', -1, 64)
}

func ShowBigfloat(x any) any { return x.(*big.Float).Text('G', -1) }
func ShowBigint(x any) any  { return x.(*big.Int).String() }
func ShowInt8(x any) any    { return fmt.Sprintf("%d", x.(int8)) }
func ShowInt16(x any) any   { return fmt.Sprintf("%d", x.(int16)) }
func ShowUint8(x any) any   { return fmt.Sprintf("%d", x.(uint8)) }
func ShowUint16(x any) any  { return fmt.Sprintf("%d", x.(uint16)) }
func ShowUint32(x any) any  { return fmt.Sprintf("%d", x.(uint32)) }
func ShowUint64(x any) any  { return fmt.Sprintf("%d", x.(uint64)) }
func ShowString_(x any) any { return strconv.Quote(x.(string)) }
func BigfloatToFloat32(x any) any { f, _ := x.(*big.Float).Float32(); return f }
func BigfloatToFloat64(x any) any { f, _ := x.(*big.Float).Float64(); return f }
func BigintToInt8(x any) any  { return int8(x.(*big.Int).Int64()) }
func BigintToInt16(x any) any { return int16(x.(*big.Int).Int64()) }
func BigintToUint8(x any) any  { return uint8(x.(*big.Int).Uint64()) }
func BigintToUint16(x any) any { return uint16(x.(*big.Int).Uint64()) }
func BigintToUint32(x any) any { return uint32(x.(*big.Int).Uint64()) }
func BigintToUint64(x any) any { return x.(*big.Int).Uint64() }
func Float32ToBigfloat(x any) any { return new(big.Float).SetFloat64(float64(x.(float32))) }
func Float64ToBigfloat(x any) any { return new(big.Float).SetFloat64(x.(float64)) }
func Int8ToBigint(x any) any  { return big.NewInt(int64(x.(int8))) }
func Int16ToBigint(x any) any { return big.NewInt(int64(x.(int16))) }
func Uint8ToBigint(x any) any  { return big.NewInt(int64(x.(uint8))) }
func Uint16ToBigint(x any) any { return big.NewInt(int64(x.(uint16))) }
func Uint32ToBigint(x any) any { return big.NewInt(int64(x.(uint32))) }
func Uint64ToBigint(x any) any { bi := new(big.Int); bi.SetUint64(x.(uint64)); return bi }
