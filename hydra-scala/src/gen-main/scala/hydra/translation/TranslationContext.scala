package hydra.translation

import hydra.evaluation.Context

case class TranslationContext(
    /**
     * @type hydra/evaluation.Context
     */
    evaluation: Context,
    
    /**
     * @type hydra/translation.Language
     */
    source: Language,
    
    /**
     * @type hydra/translation.Language
     */
    target: Language
)

val _TranslationContext: String = "hydra/translation.TranslationContext"
val _TranslationContext_evaluation: String = "evaluation"
val _TranslationContext_source: String = "source"
val _TranslationContext_target: String = "target"
