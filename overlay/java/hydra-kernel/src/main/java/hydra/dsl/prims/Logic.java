package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.logic.And;
import hydra.lib.logic.IfElse;
import hydra.lib.logic.Not;
import hydra.lib.logic.Or;

/**
 * DSL interface providing logic primitive operations.
 */
public interface Logic {
    /**
     * Returns a term representing the and primitive operation.
     * Logical conjunction.
     *
     * @return a term for logical and
     */
    static Term and() {
        return new And().term();
    }

    /**
     * Returns a term representing the ifElse primitive operation.
     * Conditional expression.
     *
     * @return a term for conditional expressions
     */
    static Term ifElse() {
        return new IfElse().term();
    }

    /**
     * Returns a term representing the not primitive operation.
     * Logical negation.
     *
     * @return a term for logical not
     */
    static Term not() {
        return new Not().term();
    }

    /**
     * Returns a term representing the or primitive operation.
     * Logical disjunction.
     *
     * @return a term for logical or
     */
    static Term or() {
        return new Or().term();
    }
}
