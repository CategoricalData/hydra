package hydra.core

enum LiteralVariant:
    case binary() extends LiteralVariant
    case boolean() extends LiteralVariant
    case float() extends LiteralVariant
    case integer() extends LiteralVariant
    case string() extends LiteralVariant

val _LiteralVariant: String = "hydra/core.LiteralVariant"
val _LiteralVariant_binary: String = "binary"
val _LiteralVariant_boolean: String = "boolean"
val _LiteralVariant_float: String = "float"
val _LiteralVariant_integer: String = "integer"
val _LiteralVariant_string: String = "string"
