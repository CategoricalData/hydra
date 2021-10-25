package hydra.core

case class MapType (
    /**
     * @type hydra/core.Type
     */
    keys: hydra.core.Type,
    
    /**
     * @type hydra/core.Type
     */
    values: hydra.core.Type
)

val _MapType: String = "hydra/core.MapType"
val _MapType_keys: String = "keys"
val _MapType_values: String = "values"
