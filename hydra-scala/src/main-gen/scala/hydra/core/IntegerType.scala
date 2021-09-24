package hydra.core

enum IntegerType:
    case bigint() extends IntegerType
    case int8() extends IntegerType
    case int16() extends IntegerType
    case int32() extends IntegerType
    case int64() extends IntegerType
    case uint8() extends IntegerType
    case uint16() extends IntegerType
    case uint32() extends IntegerType
    case uint64() extends IntegerType

val _IntegerType: String = "hydra/core.IntegerType"
val _IntegerType_bigint: String = "bigint"
val _IntegerType_int16: String = "int16"
val _IntegerType_int32: String = "int32"
val _IntegerType_int64: String = "int64"
val _IntegerType_int8: String = "int8"
val _IntegerType_uint16: String = "uint16"
val _IntegerType_uint32: String = "uint32"
val _IntegerType_uint64: String = "uint64"
val _IntegerType_uint8: String = "uint8"
