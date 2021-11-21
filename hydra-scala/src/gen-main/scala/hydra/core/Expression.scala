package hydra.core

enum Expression[a]:
    /**
     * A function application
     * 
     * @type parameterized:
     *         genericType: hydra/core.Application
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case application(value: hydra.core.Application[a]) extends Expression[a]
    /**
     * An atomic value
     * 
     * @type hydra/core.AtomicValue
     */
    case atomic(value: hydra.core.AtomicValue) extends Expression[a]
    /**
     * An element reference
     * 
     * @type hydra/core.Name
     */
    case element(value: hydra.core.Name) extends Expression[a]
    /**
     * A function term
     * 
     * @type parameterized:
     *         genericType: hydra/core.Function
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case function(value: hydra.core.Function[a]) extends Expression[a]
    /**
     * A list
     * 
     * @type list:
     *         parameterized:
     *           genericType: hydra/core.Term
     *           parameters:
     *           - type:
     *               variable: a
     *             variable: a
     */
    case list(value: Seq[hydra.core.Term[a]]) extends Expression[a]
    /**
     * A map of key terms to value terms
     * 
     * @type map:
     *         keys:
     *           parameterized:
     *             genericType: hydra/core.Term
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     *         values:
     *           parameterized:
     *             genericType: hydra/core.Term
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     */
    case map(value: Map[hydra.core.Term[a], hydra.core.Term[a]]) extends Expression[a]
    /**
     * An optional value
     * 
     * @type optional:
     *         parameterized:
     *           genericType: hydra/core.Term
     *           parameters:
     *           - type:
     *               variable: a
     *             variable: a
     */
    case optional(value: Option[hydra.core.Term[a]]) extends Expression[a]
    /**
     * A record, or labeled tuple
     * 
     * @type list:
     *         parameterized:
     *           genericType: hydra/core.Field
     *           parameters:
     *           - type:
     *               variable: a
     *             variable: a
     */
    case record(value: Seq[hydra.core.Field[a]]) extends Expression[a]
    /**
     * A set of terms
     * 
     * @type set:
     *         parameterized:
     *           genericType: hydra/core.Term
     *           parameters:
     *           - type:
     *               variable: a
     *             variable: a
     */
    case set(value: Set[hydra.core.Term[a]]) extends Expression[a]
    /**
     * A type abstraction (generalization), which binds a type variable to a term
     * 
     * @type parameterized:
     *         genericType: hydra/core.TypeAbstraction
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case typeAbstraction(value: hydra.core.TypeAbstraction[a]) extends Expression[a]
    /**
     * A type application (instantiation), which applies a term to a type
     * 
     * @type parameterized:
     *         genericType: hydra/core.TypeApplication
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case typeApplication(value: hydra.core.TypeApplication[a]) extends Expression[a]
    /**
     * A union term, i.e. a generalization of inl() or inr()
     * 
     * @type parameterized:
     *         genericType: hydra/core.Field
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case union(value: hydra.core.Field[a]) extends Expression[a]
    /**
     * A variable reference
     * 
     * @type hydra/core.Variable
     */
    case variable(value: hydra.core.Variable) extends Expression[a]

val _Expression: String = "hydra/core.Expression"
val _Expression_application: String = "application"
val _Expression_atomic: String = "atomic"
val _Expression_element: String = "element"
val _Expression_function: String = "function"
val _Expression_list: String = "list"
val _Expression_map: String = "map"
val _Expression_optional: String = "optional"
val _Expression_record: String = "record"
val _Expression_set: String = "set"
val _Expression_typeAbstraction: String = "typeAbstraction"
val _Expression_typeApplication: String = "typeApplication"
val _Expression_union: String = "union"
val _Expression_variable: String = "variable"
