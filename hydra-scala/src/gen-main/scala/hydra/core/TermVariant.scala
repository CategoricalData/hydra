package hydra.core

enum TermVariant:
    case application() extends TermVariant
    case element() extends TermVariant
    case function() extends TermVariant
    case let() extends TermVariant
    case list() extends TermVariant
    case literal() extends TermVariant
    case map() extends TermVariant
    case nominal() extends TermVariant
    case optional() extends TermVariant
    case record() extends TermVariant
    case set() extends TermVariant
    case typeAbstraction() extends TermVariant
    case typeApplication() extends TermVariant
    case union() extends TermVariant
    case variable() extends TermVariant

val _TermVariant: String = "hydra/core.TermVariant"
val _TermVariant_application: String = "application"
val _TermVariant_element: String = "element"
val _TermVariant_function: String = "function"
val _TermVariant_let: String = "let"
val _TermVariant_list: String = "list"
val _TermVariant_literal: String = "literal"
val _TermVariant_map: String = "map"
val _TermVariant_nominal: String = "nominal"
val _TermVariant_optional: String = "optional"
val _TermVariant_record: String = "record"
val _TermVariant_set: String = "set"
val _TermVariant_typeAbstraction: String = "typeAbstraction"
val _TermVariant_typeApplication: String = "typeApplication"
val _TermVariant_union: String = "union"
val _TermVariant_variable: String = "variable"
