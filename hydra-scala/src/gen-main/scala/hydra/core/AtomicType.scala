/**
 * Any of a fixed set of atomic types, also called base types, primitive types, or type constants
 * 
 * @comments The so-called term constants, or valid values, of each atomic type are unspecified
 */
package hydra.core

/**
 * Any of a fixed set of atomic types, also called base types, primitive types, or type constants
 * 
 * @comments The so-called term constants, or valid values, of each atomic type are unspecified
 */
enum AtomicType:
    case binary() extends AtomicType
    case boolean() extends AtomicType
    /**
     * @type hydra/core.FloatType
     */
    case float(value: FloatType) extends AtomicType
    /**
     * @type hydra/core.IntegerType
     */
    case integer(value: IntegerType) extends AtomicType
    case string() extends AtomicType

val _AtomicType: String = "hydra/core.AtomicType"
val _AtomicType_binary: String = "binary"
val _AtomicType_boolean: String = "boolean"
val _AtomicType_float: String = "float"
val _AtomicType_integer: String = "integer"
val _AtomicType_string: String = "string"
