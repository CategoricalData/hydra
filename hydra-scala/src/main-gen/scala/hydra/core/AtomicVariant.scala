package hydra.core

enum AtomicVariant:
    case binary() extends AtomicVariant
    case boolean() extends AtomicVariant
    case float() extends AtomicVariant
    case integer() extends AtomicVariant
    case string() extends AtomicVariant

val _AtomicVariant: String = "hydra/core.AtomicVariant"
val _AtomicVariant_binary: String = "binary"
val _AtomicVariant_boolean: String = "boolean"
val _AtomicVariant_float: String = "float"
val _AtomicVariant_integer: String = "integer"
val _AtomicVariant_string: String = "string"
