/**
 * A labeled term
 */
package hydra.core

/**
 * A labeled term
 */
case class Field[a] (
    /**
     * @type hydra/core.FieldName
     */
    name: hydra.core.FieldName,
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    term: hydra.core.Term[a]
)

val _Field: String = "hydra/core.Field"
val _Field_name: String = "name"
val _Field_term: String = "term"
