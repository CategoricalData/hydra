/**
 * A term which applies a function to an argument
 */
package hydra.core

/**
 * A term which applies a function to an argument
 */
case class Application[a] (
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    function: hydra.core.Term[a],
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    argument: hydra.core.Term[a]
)

val _Application: String = "hydra/core.Application"
val _Application_argument: String = "argument"
val _Application_function: String = "function"
