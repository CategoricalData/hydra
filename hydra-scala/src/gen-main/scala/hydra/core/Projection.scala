/**
 * A projection of a field from a record
 */
package hydra.core

/**
 * A projection of a field from a record
 */
case class Projection (
    /**
     * The projected field
     * 
     * @type hydra/core.FieldName
     */
    field: hydra.core.FieldName,
    
    /**
     * The name of the record type which defines the field
     * 
     * @comments We assume that we can only project from named record types
     * @type hydra/core.Name
     */
    context: hydra.core.Name
)

val _Projection: String = "hydra/core.Projection"
val _Projection_context: String = "context"
val _Projection_field: String = "field"
