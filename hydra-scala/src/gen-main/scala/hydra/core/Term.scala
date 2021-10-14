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
     * An element reference
     * 
     * @type hydra/core.Name
     */
    case element(value: Name) extends Term
    /**
     * A function term
     * 
     * @type hydra/core.Function
     */
    case function(value: Function) extends Term
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
val _Term_element: String = "element"
val _Term_function: String = "function"
val _Term_list: String = "list"
val _Term_map: String = "map"
val _Term_record: String = "record"
val _Term_set: String = "set"
val _Term_union: String = "union"
val _Term_variable: String = "variable"
