package hydra.core

case class FieldType (
    /**
     * @type hydra/core.FieldName
     */
    name: hydra.core.FieldName,
    
    /**
     * @type hydra/core.Type
     */
    `type`: hydra.core.Type
)

val _FieldType: String = "hydra/core.FieldType"
val _FieldType_name: String = "name"
val _FieldType_type: String = "type"
