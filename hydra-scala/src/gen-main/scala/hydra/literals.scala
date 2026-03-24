package hydra.literals

import hydra.core.*

import hydra.lib.literals

def bigfloatToFloatValue(ft: hydra.core.FloatType)(bf: BigDecimal): hydra.core.FloatValue =
  ft match
  case hydra.core.FloatType.bigfloat() => hydra.core.FloatValue.bigfloat(bf)
  case hydra.core.FloatType.float32() => hydra.core.FloatValue.float32(hydra.lib.literals.bigfloatToFloat32(bf))
  case hydra.core.FloatType.float64() => hydra.core.FloatValue.float64(hydra.lib.literals.bigfloatToFloat64(bf))

def bigintToIntegerValue(it: hydra.core.IntegerType)(bi: BigInt): hydra.core.IntegerValue =
  it match
  case hydra.core.IntegerType.bigint() => hydra.core.IntegerValue.bigint(bi)
  case hydra.core.IntegerType.int8() => hydra.core.IntegerValue.int8(hydra.lib.literals.bigintToInt8(bi))
  case hydra.core.IntegerType.int16() => hydra.core.IntegerValue.int16(hydra.lib.literals.bigintToInt16(bi))
  case hydra.core.IntegerType.int32() => hydra.core.IntegerValue.int32(hydra.lib.literals.bigintToInt32(bi))
  case hydra.core.IntegerType.int64() => hydra.core.IntegerValue.int64(hydra.lib.literals.bigintToInt64(bi))
  case hydra.core.IntegerType.uint8() => hydra.core.IntegerValue.uint8(hydra.lib.literals.bigintToUint8(bi))
  case hydra.core.IntegerType.uint16() => hydra.core.IntegerValue.uint16(hydra.lib.literals.bigintToUint16(bi))
  case hydra.core.IntegerType.uint32() => hydra.core.IntegerValue.uint32(hydra.lib.literals.bigintToUint32(bi))
  case hydra.core.IntegerType.uint64() => hydra.core.IntegerValue.uint64(hydra.lib.literals.bigintToUint64(bi))

def floatValueToBigfloat(v1: hydra.core.FloatValue): BigDecimal =
  v1 match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_bf) => v_FloatValue_bigfloat_bf
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f32) => hydra.lib.literals.float32ToBigfloat(v_FloatValue_float32_f32)
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f64) => hydra.lib.literals.float64ToBigfloat(v_FloatValue_float64_f64)

def integerValueToBigint(v1: hydra.core.IntegerValue): BigInt =
  v1 match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_bi) => v_IntegerValue_bigint_bi
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i8) => hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i8)
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i16) => hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i16)
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i32) => hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i32)
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i64) => hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i64)
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_ui8) => hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_ui8)
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_ui16) => hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_ui16)
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_ui32) => hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_ui32)
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_ui64) => hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_ui64)
