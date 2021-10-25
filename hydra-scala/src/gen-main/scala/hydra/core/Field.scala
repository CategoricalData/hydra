/**
 * A labeled term
 */
package hydra.core

/**
 * A labeled term
 */
case class Field (
    /**
     * @type hydra/core.FieldName
     */
    name: hydra.core.FieldName,
    
    /**
     * @type hydra/core.Term
     */
    term: hydra.core.Term
)

val _Field: String = "hydra/core.Field"
val _Field_name: String = "name"
val _Field_term: String = "term"
