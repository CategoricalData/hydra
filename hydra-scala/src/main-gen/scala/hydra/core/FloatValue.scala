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
