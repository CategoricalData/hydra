package hydra.core

case class TypedTerm(
    /**
     * @type hydra/core.Type
     */
    `type`: Type,
    
    /**
     * @type hydra/core.Term
     */
    term: Term
)

val _TypedTerm: String = "hydra/core.TypedTerm"
val _TypedTerm_term: String = "term"
val _TypedTerm_type: String = "type"
