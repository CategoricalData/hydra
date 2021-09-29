package hydra.core

enum TypeVariant:
    case atomic() extends TypeVariant
    case element() extends TypeVariant
    case function() extends TypeVariant
    case list() extends TypeVariant
    case map() extends TypeVariant
    case nominal() extends TypeVariant
    case record() extends TypeVariant
    case set() extends TypeVariant
    case union() extends TypeVariant

val _TypeVariant: String = "hydra/core.TypeVariant"
val _TypeVariant_atomic: String = "atomic"
val _TypeVariant_element: String = "element"
val _TypeVariant_function: String = "function"
val _TypeVariant_list: String = "list"
val _TypeVariant_map: String = "map"
val _TypeVariant_nominal: String = "nominal"
val _TypeVariant_record: String = "record"
val _TypeVariant_set: String = "set"
val _TypeVariant_union: String = "union"
