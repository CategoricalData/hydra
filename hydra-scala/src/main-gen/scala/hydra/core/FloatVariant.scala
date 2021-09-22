package hydra.core

enum FloatVariant:
    case bigfloat() extends FloatVariant
    case float32() extends FloatVariant
    case float64() extends FloatVariant
