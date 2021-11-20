package hydra.core

case class Term[a] (
    /**
     * @type parameterized:
     *         genericType: hydra/core.Expression
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    data: hydra.core.Expression[a],
    
    /**
     * @type variable: a
     */
    meta: a
)

val _Term: String = "hydra/core.Term"
val _Term_data: String = "data"
val _Term_meta: String = "meta"
