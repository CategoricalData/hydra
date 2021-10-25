/**
 * A function abstraction (lambda)
 */
package hydra.core

/**
 * A function abstraction (lambda)
 */
case class Lambda (
    /**
     * The parameter of the lambda
     * 
     * @type hydra/core.Variable
     */
    parameter: hydra.core.Variable,
    
    /**
     * The body of the lambda
     * 
     * @type hydra/core.Term
     */
    body: hydra.core.Term
)

val _Lambda: String = "hydra/core.Lambda"
val _Lambda_body: String = "body"
val _Lambda_parameter: String = "parameter"
