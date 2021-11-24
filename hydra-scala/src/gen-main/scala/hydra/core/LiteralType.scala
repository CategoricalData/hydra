/**
 * Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants
 * 
 * @comments The so-called term constants, or valid values, of each literal type are unspecified
 */
package hydra.core

/**
 * Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants
 * 
 * @comments The so-called term constants, or valid values, of each literal type are unspecified
 */
enum LiteralType:
    case binary() extends LiteralType
    case boolean() extends LiteralType
    /**
     * @type hydra/core.FloatType
     */
    case float(value: hydra.core.FloatType) extends LiteralType
    /**
     * @type hydra/core.IntegerType
     */
    case integer(value: hydra.core.IntegerType) extends LiteralType
    case string() extends LiteralType

val _LiteralType: String = "hydra/core.LiteralType"
val _LiteralType_binary: String = "binary"
val _LiteralType_boolean: String = "boolean"
val _LiteralType_float: String = "float"
val _LiteralType_integer: String = "integer"
val _LiteralType_string: String = "string"
