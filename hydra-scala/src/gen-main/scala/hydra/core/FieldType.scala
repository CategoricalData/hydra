package hydra.core

case class FieldType(
    /**
     * @type hydra/core.FieldName
     */
    name: FieldName,
    
    /**
     * @type hydra/core.Type
     */
    `type`: Type
)

val _FieldType: String = "hydra/core.FieldType"
val _FieldType_name: String = "name"
val _FieldType_type: String = "type"
