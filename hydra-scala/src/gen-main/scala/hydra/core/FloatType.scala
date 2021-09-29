package hydra.core

enum FloatType:
    case bigfloat() extends FloatType
    case float32() extends FloatType
    case float64() extends FloatType

val _FloatType: String = "hydra/core.FloatType"
val _FloatType_bigfloat: String = "bigfloat"
val _FloatType_float32: String = "float32"
val _FloatType_float64: String = "float64"
