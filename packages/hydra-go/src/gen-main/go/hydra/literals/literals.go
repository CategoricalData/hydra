// Note: this is an automatically generated file. Do not edit.

package literals

import (
  "hydra.dev/hydra/core"
  libliterals "hydra.dev/hydra/lib/literals"
  "math/big"
)

func BigfloatToFloatValue (ft core.FloatType, bf float64) core.FloatValue {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatTypeBigfloat:
      return func (_ struct{}) any {
        return core.FloatValueBigfloat{Value: bf}
      }(v)
      case core.FloatTypeFloat32_:
      return func (_ struct{}) any {
        return core.FloatValueFloat32_{Value: libliterals.BigfloatToFloat32(bf).(float32)}
      }(v)
      case core.FloatTypeFloat64_:
      return func (_ struct{}) any {
        return core.FloatValueFloat64_{Value: libliterals.BigfloatToFloat64(bf).(float64)}
      }(v)
    }
    return nil
  }(ft).(core.FloatValue)
}

func BigintToIntegerValue (it core.IntegerType, bi *big.Int) core.IntegerValue {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerTypeBigint:
      return func (_ struct{}) any {
        return core.IntegerValueBigint{Value: bi}
      }(v)
      case core.IntegerTypeInt8_:
      return func (_ struct{}) any {
        return core.IntegerValueInt8_{Value: libliterals.BigintToInt8(bi).(int8)}
      }(v)
      case core.IntegerTypeInt16_:
      return func (_ struct{}) any {
        return core.IntegerValueInt16_{Value: libliterals.BigintToInt16(bi).(int16)}
      }(v)
      case core.IntegerTypeInt32_:
      return func (_ struct{}) any {
        return core.IntegerValueInt32_{Value: libliterals.BigintToInt32(bi).(int32)}
      }(v)
      case core.IntegerTypeInt64_:
      return func (_ struct{}) any {
        return core.IntegerValueInt64_{Value: libliterals.BigintToInt64(bi).(int64)}
      }(v)
      case core.IntegerTypeUint8_:
      return func (_ struct{}) any {
        return core.IntegerValueUint8_{Value: libliterals.BigintToUint8(bi).(uint8)}
      }(v)
      case core.IntegerTypeUint16_:
      return func (_ struct{}) any {
        return core.IntegerValueUint16_{Value: libliterals.BigintToUint16(bi).(uint16)}
      }(v)
      case core.IntegerTypeUint32_:
      return func (_ struct{}) any {
        return core.IntegerValueUint32_{Value: libliterals.BigintToUint32(bi).(uint32)}
      }(v)
      case core.IntegerTypeUint64_:
      return func (_ struct{}) any {
        return core.IntegerValueUint64_{Value: libliterals.BigintToUint64(bi).(uint64)}
      }(v)
    }
    return nil
  }(it).(core.IntegerValue)
}

func FloatValueToBigfloat (v1 core.FloatValue) float64 {
  return func (x any) any {
    switch v := x.(type) {
      case core.FloatValueBigfloat:
      return func (bf float64) any {
        return bf
      }(v.Value)
      case core.FloatValueFloat32_:
      return func (f32 float32) any {
        return libliterals.Float32ToBigfloat(f32)
      }(v.Value)
      case core.FloatValueFloat64_:
      return func (f64 float64) any {
        return libliterals.Float64ToBigfloat(f64)
      }(v.Value)
    }
    return nil
  }(v1).(float64)
}

func IntegerValueToBigint (v1 core.IntegerValue) *big.Int {
  return func (x any) any {
    switch v := x.(type) {
      case core.IntegerValueBigint:
      return func (bi *big.Int) any {
        return bi
      }(v.Value)
      case core.IntegerValueInt8_:
      return func (i8 int8) any {
        return libliterals.Int8ToBigint(i8)
      }(v.Value)
      case core.IntegerValueInt16_:
      return func (i16 int16) any {
        return libliterals.Int16ToBigint(i16)
      }(v.Value)
      case core.IntegerValueInt32_:
      return func (i32 int32) any {
        return libliterals.Int32ToBigint(i32)
      }(v.Value)
      case core.IntegerValueInt64_:
      return func (i64 int64) any {
        return libliterals.Int64ToBigint(i64)
      }(v.Value)
      case core.IntegerValueUint8_:
      return func (ui8 uint8) any {
        return libliterals.Uint8ToBigint(ui8)
      }(v.Value)
      case core.IntegerValueUint16_:
      return func (ui16 uint16) any {
        return libliterals.Uint16ToBigint(ui16)
      }(v.Value)
      case core.IntegerValueUint32_:
      return func (ui32 uint32) any {
        return libliterals.Uint32ToBigint(ui32)
      }(v.Value)
      case core.IntegerValueUint64_:
      return func (ui64 uint64) any {
        return libliterals.Uint64ToBigint(ui64)
      }(v.Value)
    }
    return nil
  }(v1).(*big.Int)
}
