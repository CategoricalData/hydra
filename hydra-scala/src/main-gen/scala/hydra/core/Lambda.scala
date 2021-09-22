/**
 * A function abstraction (lambda)
 */
package hydra.core

/**
 * A function abstraction (lambda)
 */
case class Lambda(
    /**
     * The parameter of the lambda
     * 
     * @type hydra/core.Variable
     */
    parameter: Variable,
    
    /**
     * The body of the lambda
     * 
     * @type hydra/core.Term
     */
    body: Term
)
