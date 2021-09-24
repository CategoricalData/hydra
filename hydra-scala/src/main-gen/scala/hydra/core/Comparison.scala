/**
 * An equality judgement: less than, equal to, or greater than
 */
package hydra.core

/**
 * An equality judgement: less than, equal to, or greater than
 */
enum Comparison:
    case lessThan() extends Comparison
    case equalTo() extends Comparison
    case greaterThan() extends Comparison

val _Comparison: String = "hydra/core.Comparison"
val _Comparison_equalTo: String = "equalTo"
val _Comparison_greaterThan: String = "greaterThan"
val _Comparison_lessThan: String = "lessThan"
