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
