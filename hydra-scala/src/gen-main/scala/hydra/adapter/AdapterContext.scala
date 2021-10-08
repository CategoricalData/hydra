package hydra.adapter

import hydra.evaluation.Context

case class AdapterContext(
    /**
     * @type hydra/evaluation.Context
     */
    evaluation: Context,
    
    /**
     * @type hydra/adapter.Language
     */
    source: Language,
    
    /**
     * @type hydra/adapter.Language
     */
    target: Language
)

val _AdapterContext: String = "hydra/adapter.AdapterContext"
val _AdapterContext_evaluation: String = "evaluation"
val _AdapterContext_source: String = "source"
val _AdapterContext_target: String = "target"
