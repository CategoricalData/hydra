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
