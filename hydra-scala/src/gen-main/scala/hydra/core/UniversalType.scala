/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
package hydra.core

/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
case class UniversalType (
    /**
     * @type hydra/core.TypeVariable
     */
    variable: hydra.core.TypeVariable,
    
    /**
     * @type hydra/core.Type
     */
    body: hydra.core.Type
)

val _UniversalType: String = "hydra/core.UniversalType"
val _UniversalType_body: String = "body"
val _UniversalType_variable: String = "variable"
