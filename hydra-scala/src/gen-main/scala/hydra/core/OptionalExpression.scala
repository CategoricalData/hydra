/**
 * An encoded optional value, for languages which do not natively support optionals
 */
package hydra.core

/**
 * An encoded optional value, for languages which do not natively support optionals
 */
enum OptionalExpression[a]:
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    case just(value: hydra.core.Term[a]) extends OptionalExpression[a]
    case nothing() extends OptionalExpression[a]

val _OptionalExpression: String = "hydra/core.OptionalExpression"
val _OptionalExpression_just: String = "just"
val _OptionalExpression_nothing: String = "nothing"
