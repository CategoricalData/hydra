package hydra.evaluation

case class EvaluationStrategy (
    /**
     * Whether a term of a given variant is considered to be fully reduced, without further inspection
     * 
     * @type set: hydra/core.TermVariant
     */
    opaqueTermVariants: Set[hydra.core.TermVariant]
)

val _EvaluationStrategy: String = "hydra/evaluation.EvaluationStrategy"
val _EvaluationStrategy_opaqueTermVariants: String = "opaqueTermVariants"
