/**
 * A term which applies a function to an argument
 */
package hydra.core

/**
 * A term which applies a function to an argument
 */
case class Application(
    /**
     * @type hydra/core.Term
     */
    function: Term,
    
    /**
     * @type hydra/core.Term
     */
    argument: Term
)

val _Application: String = "hydra/core.Application"
val _Application_argument: String = "argument"
val _Application_function: String = "function"
