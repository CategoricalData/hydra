/**
 * A variant expression, or instance of a union type
 */
package hydra.core

/**
 * A variant expression, or instance of a union type
 */
case class UnionExpression[a] (
    /**
     * The name of the type of this union expression
     * 
     * @comments We assume that we can only instantiate named union types
     * @type hydra/core.Name
     */
    context: hydra.core.Name,
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Field
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    field: hydra.core.Field[a]
)

val _UnionExpression: String = "hydra/core.UnionExpression"
val _UnionExpression_context: String = "context"
val _UnionExpression_field: String = "field"
