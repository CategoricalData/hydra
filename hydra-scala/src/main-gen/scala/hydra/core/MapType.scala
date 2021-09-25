package hydra.core

case class MapType(
    /**
     * @type hydra/core.Type
     */
    keys: Type,
    
    /**
     * @type hydra/core.Type
     */
    values: Type
)

val _MapType: String = "hydra/core.MapType"
val _MapType_keys: String = "keys"
val _MapType_values: String = "values"
