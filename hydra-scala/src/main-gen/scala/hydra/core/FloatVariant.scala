package hydra.core

enum FloatVariant:
    case bigfloat() extends FloatVariant
    case float32() extends FloatVariant
    case float64() extends FloatVariant

val _FloatVariant: String = "hydra/core.FloatVariant"
val _FloatVariant_bigfloat: String = "bigfloat"
val _FloatVariant_float32: String = "float32"
val _FloatVariant_float64: String = "float64"
