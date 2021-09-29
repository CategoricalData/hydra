package hydra.evaluation

import hydra.core.TermVariant

case class EvaluationStrategy(
    /**
     * Whether a term of a given variant is considered to be fully reduced, without further inspection
     * 
     * @type set: hydra/core.TermVariant
     */
    opaqueTermVariants: Set[TermVariant]
)

val _EvaluationStrategy: String = "hydra/evaluation.EvaluationStrategy"
val _EvaluationStrategy_opaqueTermVariants: String = "opaqueTermVariants"
