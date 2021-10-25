package hydra.adapter

case class AdapterContext (
    /**
     * @type hydra/evaluation.Context
     */
    evaluation: hydra.evaluation.Context,
    
    /**
     * @type hydra/adapter.Language
     */
    source: hydra.adapter.Language,
    
    /**
     * @type hydra/adapter.Language
     */
    target: hydra.adapter.Language
)

val _AdapterContext: String = "hydra/adapter.AdapterContext"
val _AdapterContext_evaluation: String = "evaluation"
val _AdapterContext_source: String = "source"
val _AdapterContext_target: String = "target"
