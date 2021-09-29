package hydra.core

enum BooleanValue:
    case `false`() extends BooleanValue
    case `true`() extends BooleanValue

val _BooleanValue: String = "hydra/core.BooleanValue"
val _BooleanValue_false: String = "false"
val _BooleanValue_true: String = "true"
