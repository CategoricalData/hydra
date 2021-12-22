/**
 * A helper object for specifying and unmarshalling an argument to a primitive function
 */
package hydra.evaluation

/**
 * A helper object for specifying and unmarshalling an argument to a primitive function
 */
case class InputSpec[a, m] (
    /**
     * @type hydra/core.Type
     */
    `type`: hydra.core.Type,
    
    /**
     * @type function:
     *         from:
     *         - parameterized:
     *             genericType: hydra/core.Term
     *             parameters:
     *             - type:
     *                 variable: m
     *               variable: a
     *         to:
     *           parameterized:
     *             genericType: hydra/evaluation.Result
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     */
    unmarshal: hydra.core.Term[m] => hydra.evaluation.Result[a]
)

val _InputSpec: String = "hydra/evaluation.InputSpec"
val _InputSpec_type: String = "type"
val _InputSpec_unmarshal: String = "unmarshal"
