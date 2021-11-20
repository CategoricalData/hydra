package hydra.core

case class TypedTerm[a] (
    /**
     * @type hydra/core.Type
     */
    `type`: hydra.core.Type,
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    term: hydra.core.Term[a]
)

val _TypedTerm: String = "hydra/core.TypedTerm"
val _TypedTerm_term: String = "term"
val _TypedTerm_type: String = "type"
