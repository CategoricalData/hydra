/**
 * A term constant; an instance of a literal type
 */
package hydra.core

/**
 * A term constant; an instance of a literal type
 */
enum Literal:
    /**
     * @type binary
     */
    case binary(value: String) extends Literal
    /**
     * @type hydra/core.BooleanValue
     */
    case boolean(value: hydra.core.BooleanValue) extends Literal
    /**
     * @type hydra/core.FloatValue
     */
    case float(value: hydra.core.FloatValue) extends Literal
    /**
     * @type hydra/core.IntegerValue
     */
    case integer(value: hydra.core.IntegerValue) extends Literal
    /**
     * @type string
     */
    case string(value: String) extends Literal

val _Literal: String = "hydra/core.Literal"
val _Literal_binary: String = "binary"
val _Literal_boolean: String = "boolean"
val _Literal_float: String = "float"
val _Literal_integer: String = "integer"
val _Literal_string: String = "string"
