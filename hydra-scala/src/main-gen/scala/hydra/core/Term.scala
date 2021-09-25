package hydra.core

enum Term:
    /**
     * A function application
     * 
     * @type hydra/core.Application
     */
    case application(value: Application) extends Term
    /**
     * An atomic value
     * 
     * @type hydra/core.AtomicValue
     */
    case atomic(value: AtomicValue) extends Term
    /**
     * A case statement applied to a variant record, consisting of a function term for each alternative in the union
     * 
     * @type list: hydra/core.Field
     */
    case cases(value: Seq[Field]) extends Term
    /**
     * Compares a term with a given term of the same type, producing a Comparison
     * 
     * @type hydra/core.Term
     */
    case compareTo(value: Term) extends Term
    /**
     * Hydra's delta function, which maps an element to its data term
     */
    case data() extends Term
    /**
     * An element reference
     * 
     * @type hydra/core.Name
     */
    case element(value: Name) extends Term
    /**
     * A reference to a built-in function
     * 
     * @type hydra/core.Name
     */
    case function(value: Name) extends Term
    /**
     * A function abstraction (lambda)
     * 
     * @type hydra/core.Lambda
     */
    case lambda(value: Lambda) extends Term
    /**
     * A list
     * 
     * @type list: hydra/core.Term
     */
    case list(value: Seq[Term]) extends Term
    /**
     * A map of key terms to value terms
     * 
     * @type map:
     *         keys: hydra/core.Term
     *         values: hydra/core.Term
     */
    case map(value: Map[Term, Term]) extends Term
    /**
     * A projection of a field from a record
     * 
     * @type hydra/core.FieldName
     */
    case projection(value: FieldName) extends Term
    /**
     * A record, or labeled tuple
     * 
     * @type list: hydra/core.Field
     */
    case record(value: Seq[Field]) extends Term
    /**
     * A set of terms
     * 
     * @type set: hydra/core.Term
     */
    case set(value: Set[Term]) extends Term
    /**
     * A union term, i.e. a generalization of inl() or inr()
     * 
     * @type hydra/core.Field
     */
    case union(value: Field) extends Term
    /**
     * A variable reference
     * 
     * @type hydra/core.Variable
     */
    case variable(value: Variable) extends Term

val _Term: String = "hydra/core.Term"
val _Term_application: String = "application"
val _Term_atomic: String = "atomic"
val _Term_cases: String = "cases"
val _Term_compareTo: String = "compareTo"
val _Term_data: String = "data"
val _Term_element: String = "element"
val _Term_function: String = "function"
val _Term_lambda: String = "lambda"
val _Term_list: String = "list"
val _Term_map: String = "map"
val _Term_projection: String = "projection"
val _Term_record: String = "record"
val _Term_set: String = "set"
val _Term_union: String = "union"
val _Term_variable: String = "variable"
