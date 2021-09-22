/**
 * A function type, also known as an arrow type
 */
package hydra.core

/**
 * A function type, also known as an arrow type
 */
case class FunctionType(
    /**
     * @type hydra/core.Type
     */
    domain: Type,
    
    /**
     * @type hydra/core.Type
     */
    codomain: Type
)
