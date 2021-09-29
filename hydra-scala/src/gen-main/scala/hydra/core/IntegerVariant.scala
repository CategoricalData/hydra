package hydra.core

enum IntegerVariant:
    case bigint() extends IntegerVariant
    case int8() extends IntegerVariant
    case int16() extends IntegerVariant
    case int32() extends IntegerVariant
    case int64() extends IntegerVariant
    case uint8() extends IntegerVariant
    case uint16() extends IntegerVariant
    case uint32() extends IntegerVariant
    case uint64() extends IntegerVariant

val _IntegerVariant: String = "hydra/core.IntegerVariant"
val _IntegerVariant_bigint: String = "bigint"
val _IntegerVariant_int16: String = "int16"
val _IntegerVariant_int32: String = "int32"
val _IntegerVariant_int64: String = "int64"
val _IntegerVariant_int8: String = "int8"
val _IntegerVariant_uint16: String = "uint16"
val _IntegerVariant_uint32: String = "uint32"
val _IntegerVariant_uint64: String = "uint64"
val _IntegerVariant_uint8: String = "uint8"
