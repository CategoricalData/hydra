package hydra.core

case class TypeScheme (
    /**
     * @type list: hydra/core.TypeVariable
     */
    variables: Seq[hydra.core.TypeVariable],
    
    /**
     * @type hydra/core.Type
     */
    `type`: hydra.core.Type
)

val _TypeScheme: String = "hydra/core.TypeScheme"
val _TypeScheme_type: String = "type"
val _TypeScheme_variables: String = "variables"
