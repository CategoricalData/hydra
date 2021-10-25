/**
 * A function type, also known as an arrow type
 */
package hydra.core

/**
 * A function type, also known as an arrow type
 */
case class FunctionType (
    /**
     * @type hydra/core.Type
     */
    domain: hydra.core.Type,
    
    /**
     * @type hydra/core.Type
     */
    codomain: hydra.core.Type
)

val _FunctionType: String = "hydra/core.FunctionType"
val _FunctionType_codomain: String = "codomain"
val _FunctionType_domain: String = "domain"
