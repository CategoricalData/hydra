/**
 * A labeled term
 */
package hydra.core

/**
 * A labeled term
 */
case class Field(
    /**
     * @type hydra/core.FieldName
     */
    name: FieldName,
    
    /**
     * @type hydra/core.Term
     */
    term: Term
)

val _Field: String = "hydra/core.Field"
val _Field_name: String = "name"
val _Field_term: String = "term"
