package hydra.core

enum FloatValue:
    /**
     * @type float:
     *         precision: arbitrary
     */
    case bigfloat(value: Double) extends FloatValue
    /**
     * @type float
     */
    case float32(value: Float) extends FloatValue
    /**
     * @type float:
     *         precision:
     *           bits: 64
     */
    case float64(value: Double) extends FloatValue

val _FloatValue: String = "hydra/core.FloatValue"
val _FloatValue_bigfloat: String = "bigfloat"
val _FloatValue_float32: String = "float32"
val _FloatValue_float64: String = "float64"
