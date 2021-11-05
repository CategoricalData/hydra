/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
package hydra.core

/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
case class AbstractType (
    /**
     * @type hydra/core.TypeVariable
     */
    variable: hydra.core.TypeVariable,
    
    /**
     * @type hydra/core.Type
     */
    body: hydra.core.Type
)

val _AbstractType: String = "hydra/core.AbstractType"
val _AbstractType_body: String = "body"
val _AbstractType_variable: String = "variable"
