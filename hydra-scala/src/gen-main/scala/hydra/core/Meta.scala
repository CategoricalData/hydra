/**
 * A built-in metadata container for terms
 */
package hydra.core

/**
 * A built-in metadata container for terms
 */
case class Meta (
    /**
     * An optional description associated with the term
     * 
     * @type optional: string
     */
    description: Option[String],
    
    /**
     * An optional type annotation associated with the term. This may be used as a hint to the type inferencer and/or to
     * code generators.
     * 
     * @type optional: hydra/core.Type
     */
    `type`: Option[hydra.core.Type]
)

val _Meta: String = "hydra/core.Meta"
val _Meta_description: String = "description"
val _Meta_type: String = "type"
