package hydra.traversal

enum StepDirection:
    case out() extends StepDirection
    case in() extends StepDirection

val _StepDirection: String = "hydra/traversal.StepDirection"
val _StepDirection_in: String = "in"
val _StepDirection_out: String = "out"
