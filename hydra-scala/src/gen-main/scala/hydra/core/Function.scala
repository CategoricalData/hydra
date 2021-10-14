package hydra.core

enum Function:
    /**
     * A case statement applied to a variant record, consisting of a function term for each alternative in the union
     * 
     * @type list: hydra/core.Field
     */
    case cases(value: Seq[Field]) extends Function
    /**
     * Compares a term with a given term of the same type, producing a Comparison
     * 
     * @type hydra/core.Term
     */
    case compareTo(value: Term) extends Function
    /**
     * Hydra's delta function, which maps an element to its data term
     */
    case data() extends Function
    /**
     * A function abstraction (lambda)
     * 
     * @type hydra/core.Lambda
     */
    case lambda(value: Lambda) extends Function
    /**
     * A reference to a built-in (primitive) function
     * 
     * @type hydra/core.Name
     */
    case primitive(value: Name) extends Function
    /**
     * A projection of a field from a record
     * 
     * @type hydra/core.FieldName
     */
    case projection(value: FieldName) extends Function

val _Function: String = "hydra/core.Function"
val _Function_cases: String = "cases"
val _Function_compareTo: String = "compareTo"
val _Function_data: String = "data"
val _Function_lambda: String = "lambda"
val _Function_primitive: String = "primitive"
val _Function_projection: String = "projection"
