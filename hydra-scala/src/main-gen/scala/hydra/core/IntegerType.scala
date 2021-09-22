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
