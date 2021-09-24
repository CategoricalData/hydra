package hydra.core

enum IntegerValue:
    /**
     * @type integer:
     *         precision: arbitrary
     */
    case bigint(value: Long) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 8
     */
    case int8(value: Byte) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 16
     */
    case int16(value: Short) extends IntegerValue
    /**
     * @type integer
     */
    case int32(value: Int) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 64
     */
    case int64(value: Long) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 8
     *         signed: false
     */
    case uint8(value: Byte) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 16
     *         signed: false
     */
    case uint16(value: Short) extends IntegerValue
    /**
     * @type integer:
     *         signed: false
     */
    case uint32(value: Int) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 64
     *         signed: false
     */
    case uint64(value: Long) extends IntegerValue

val _IntegerValue: String = "hydra/core.IntegerValue"
val _IntegerValue_bigint: String = "bigint"
val _IntegerValue_int16: String = "int16"
val _IntegerValue_int32: String = "int32"
val _IntegerValue_int64: String = "int64"
val _IntegerValue_int8: String = "int8"
val _IntegerValue_uint16: String = "uint16"
val _IntegerValue_uint32: String = "uint32"
val _IntegerValue_uint64: String = "uint64"
val _IntegerValue_uint8: String = "uint8"
