/**
 * A type abstraction (generalization), which binds a type variable to a term
 */
package hydra.core

/**
 * A type abstraction (generalization), which binds a type variable to a term
 */
case class TypeAbstraction[a] (
    /**
     * The parameter of the abstraction
     * 
     * @type hydra/core.TypeVariable
     */
    parameter: hydra.core.TypeVariable,
    
    /**
     * The body of the abstraction
     * 
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    body: hydra.core.Term[a]
)

val _TypeAbstraction: String = "hydra/core.TypeAbstraction"
val _TypeAbstraction_body: String = "body"
val _TypeAbstraction_parameter: String = "parameter"
