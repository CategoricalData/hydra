package hydra.core

enum TermVariant:
    case application() extends TermVariant
    case atomic() extends TermVariant
    case cases() extends TermVariant
    case compareTo() extends TermVariant
    case data() extends TermVariant
    case element() extends TermVariant
    case function() extends TermVariant
    case lambda() extends TermVariant
    case list() extends TermVariant
    case map() extends TermVariant
    case projection() extends TermVariant
    case record() extends TermVariant
    case set() extends TermVariant
    case union() extends TermVariant
    case variable() extends TermVariant

val _TermVariant: String = "hydra/core.TermVariant"
val _TermVariant_application: String = "application"
val _TermVariant_atomic: String = "atomic"
val _TermVariant_cases: String = "cases"
val _TermVariant_compareTo: String = "compareTo"
val _TermVariant_data: String = "data"
val _TermVariant_element: String = "element"
val _TermVariant_function: String = "function"
val _TermVariant_lambda: String = "lambda"
val _TermVariant_list: String = "list"
val _TermVariant_map: String = "map"
val _TermVariant_projection: String = "projection"
val _TermVariant_record: String = "record"
val _TermVariant_set: String = "set"
val _TermVariant_union: String = "union"
val _TermVariant_variable: String = "variable"
