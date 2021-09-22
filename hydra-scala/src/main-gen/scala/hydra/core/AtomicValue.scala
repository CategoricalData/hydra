/**
 * A term constant; an instance of an atomic type
 */
package hydra.core

/**
 * A term constant; an instance of an atomic type
 */
enum AtomicValue:
    /**
     * @type binary
     */
    case binary(value: String) extends AtomicValue
    /**
     * @type hydra/core.BooleanValue
     */
    case boolean(value: BooleanValue) extends AtomicValue
    /**
     * @type hydra/core.FloatValue
     */
    case float(value: FloatValue) extends AtomicValue
    /**
     * @type hydra/core.IntegerValue
     */
    case integer(value: IntegerValue) extends AtomicValue
    /**
     * @type string
     */
    case string(value: String) extends AtomicValue
