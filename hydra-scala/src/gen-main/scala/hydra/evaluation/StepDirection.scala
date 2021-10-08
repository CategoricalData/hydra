package hydra.evaluation

enum StepDirection:
    case out() extends StepDirection
    case in() extends StepDirection

val _StepDirection: String = "hydra/evaluation.StepDirection"
val _StepDirection_in: String = "in"
val _StepDirection_out: String = "out"
