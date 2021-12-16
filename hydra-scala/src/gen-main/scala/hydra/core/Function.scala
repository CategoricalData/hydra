package hydra.core

enum Function[a]:
    /**
     * A case statement applied to a variant record, consisting of a function term for each alternative in the union
     * 
     * @type list:
     *         parameterized:
     *           genericType: hydra/core.Field
     *           parameters:
     *           - type:
     *               variable: a
     *             variable: a
     */
    case cases(value: Seq[hydra.core.Field[a]]) extends Function[a]
    /**
     * Compares a term with a given term of the same type, producing a Comparison
     * 
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case compareTo(value: hydra.core.Term[a]) extends Function[a]
    /**
     * Hydra's delta function, which maps an element to its data term
     */
    case data() extends Function[a]
    /**
     * A function abstraction (lambda)
     * 
     * @type parameterized:
     *         genericType: hydra/core.Lambda
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case lambda(value: hydra.core.Lambda[a]) extends Function[a]
    /**
     * Eliminator for optional terms
     * 
     * @type parameterized:
     *         genericType: hydra/core.OptionalCases
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case optionalCases(value: hydra.core.OptionalCases[a]) extends Function[a]
    /**
     * A reference to a built-in (primitive) function
     * 
     * @type hydra/core.Name
     */
    case primitive(value: hydra.core.Name) extends Function[a]
    /**
     * A projection of a field from a record
     * 
     * @type hydra/core.FieldName
     */
    case projection(value: hydra.core.FieldName) extends Function[a]

val _Function: String = "hydra/core.Function"
val _Function_cases: String = "cases"
val _Function_compareTo: String = "compareTo"
val _Function_data: String = "data"
val _Function_lambda: String = "lambda"
val _Function_optionalCases: String = "optionalCases"
val _Function_primitive: String = "primitive"
val _Function_projection: String = "projection"
