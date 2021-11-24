/**
 * A 'let' binding
 */
package hydra.core

/**
 * A 'let' binding
 */
case class Let[a] (
    /**
     * @type hydra/core.Variable
     */
    key: hydra.core.Variable,
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    value: hydra.core.Term[a],
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    environment: hydra.core.Term[a]
)

val _Let: String = "hydra/core.Let"
val _Let_environment: String = "environment"
val _Let_key: String = "key"
val _Let_value: String = "value"
