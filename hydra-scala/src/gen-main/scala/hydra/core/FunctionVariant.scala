package hydra.core

enum FunctionVariant:
    case cases() extends FunctionVariant
    case compareTo() extends FunctionVariant
    case data() extends FunctionVariant
    case lambda() extends FunctionVariant
    case optionalCases() extends FunctionVariant
    case primitive() extends FunctionVariant
    case projection() extends FunctionVariant

val _FunctionVariant: String = "hydra/core.FunctionVariant"
val _FunctionVariant_cases: String = "cases"
val _FunctionVariant_compareTo: String = "compareTo"
val _FunctionVariant_data: String = "data"
val _FunctionVariant_lambda: String = "lambda"
val _FunctionVariant_optionalCases: String = "optionalCases"
val _FunctionVariant_primitive: String = "primitive"
val _FunctionVariant_projection: String = "projection"
