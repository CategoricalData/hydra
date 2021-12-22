/**
 * A helper object for specifying and marshalling the output of a primitive function
 */
package hydra.evaluation

/**
 * A helper object for specifying and marshalling the output of a primitive function
 */
case class OutputSpec[a, m] (
    /**
     * @type hydra/core.Type
     */
    `type`: hydra.core.Type,
    
    /**
     * @type function:
     *         from:
     *         - variable: a
     *         to:
     *           parameterized:
     *             genericType: hydra/core.Term
     *             parameters:
     *             - type:
     *                 variable: m
     *               variable: a
     */
    marshal: a => hydra.core.Term[m]
)

val _OutputSpec: String = "hydra/evaluation.OutputSpec"
val _OutputSpec_marshal: String = "marshal"
val _OutputSpec_type: String = "type"
