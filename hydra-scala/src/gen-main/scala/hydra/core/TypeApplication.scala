/**
 * A type application (instantiation), which applies a term to a type
 */
package hydra.core

/**
 * A type application (instantiation), which applies a term to a type
 */
case class TypeApplication[a] (
    /**
     * A term which is the left-hand side of the application
     * 
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    function: hydra.core.Term[a],
    
    /**
     * A type which is the right-hand side of the application
     * 
     * @type hydra/core.Type
     */
    argument: hydra.core.Type
)

val _TypeApplication: String = "hydra/core.TypeApplication"
val _TypeApplication_argument: String = "argument"
val _TypeApplication_function: String = "function"
