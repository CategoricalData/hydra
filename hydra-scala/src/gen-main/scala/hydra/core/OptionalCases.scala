package hydra.core

case class OptionalCases[a] (
    /**
     * A term provided if the optional value is nothing
     * 
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    nothing: hydra.core.Term[a],
    
    /**
     * A function which is applied of the optional value is non-nothing
     * 
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    just: hydra.core.Term[a]
)

val _OptionalCases: String = "hydra/core.OptionalCases"
val _OptionalCases_just: String = "just"
val _OptionalCases_nothing: String = "nothing"
