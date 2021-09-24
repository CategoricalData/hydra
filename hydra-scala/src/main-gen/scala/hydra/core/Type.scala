package hydra.core

enum Type:
    /**
     * @type hydra/core.AtomicType
     */
    case atomic(value: AtomicType) extends Type
    /**
     * @type hydra/core.Type
     */
    case element(value: Type) extends Type
    /**
     * @type hydra/core.FunctionType
     */
    case function(value: FunctionType) extends Type
    /**
     * @type hydra/core.Type
     */
    case list(value: Type) extends Type
    /**
     * @type hydra/core.Name
     */
    case nominal(value: Name) extends Type
    /**
     * @type list: hydra/core.FieldType
     */
    case record(value: Seq[FieldType]) extends Type
    /**
     * @type list: hydra/core.FieldType
     */
    case union(value: Seq[FieldType]) extends Type

val _Type: String = "hydra/core.Type"
val _Type_atomic: String = "atomic"
val _Type_element: String = "element"
val _Type_function: String = "function"
val _Type_list: String = "list"
val _Type_nominal: String = "nominal"
val _Type_record: String = "record"
val _Type_union: String = "union"
