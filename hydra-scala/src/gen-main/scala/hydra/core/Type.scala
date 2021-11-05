package hydra.core

enum Type:
    /**
     * @type hydra/core.AtomicType
     */
    case atomic(value: hydra.core.AtomicType) extends Type
    /**
     * @type hydra/core.Type
     */
    case element(value: hydra.core.Type) extends Type
    /**
     * @type hydra/core.FunctionType
     */
    case function(value: hydra.core.FunctionType) extends Type
    /**
     * @type hydra/core.Type
     */
    case list(value: hydra.core.Type) extends Type
    /**
     * @type hydra/core.MapType
     */
    case map(value: hydra.core.MapType) extends Type
    /**
     * @type hydra/core.Name
     */
    case nominal(value: hydra.core.Name) extends Type
    /**
     * @type hydra/core.Type
     */
    case optional(value: hydra.core.Type) extends Type
    /**
     * @type list: hydra/core.FieldType
     */
    case record(value: Seq[hydra.core.FieldType]) extends Type
    /**
     * @type hydra/core.Type
     */
    case set(value: hydra.core.Type) extends Type
    /**
     * @type list: hydra/core.FieldType
     */
    case union(value: Seq[hydra.core.FieldType]) extends Type
    /**
     * @type hydra/core.UniversalType
     */
    case universal(value: hydra.core.UniversalType) extends Type
    /**
     * @type hydra/core.TypeVariable
     */
    case variable(value: hydra.core.TypeVariable) extends Type

val _Type: String = "hydra/core.Type"
val _Type_atomic: String = "atomic"
val _Type_element: String = "element"
val _Type_function: String = "function"
val _Type_list: String = "list"
val _Type_map: String = "map"
val _Type_nominal: String = "nominal"
val _Type_optional: String = "optional"
val _Type_record: String = "record"
val _Type_set: String = "set"
val _Type_union: String = "union"
val _Type_universal: String = "universal"
val _Type_variable: String = "variable"
