package hydra.core

enum FloatType:
    case bigfloat() extends FloatType
    case float32() extends FloatType
    case float64() extends FloatType
