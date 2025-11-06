package hydra.lib.equality;

import hydra.core.Term;
import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two terms.
 */
public class EqualTerm extends EqualityFunction<Term> {
    public EqualTerm() {
        super(PrimitiveType.term(), Relation.EQUALS);
    }

    /**
     * Applies the EqualTerm operation.
     * @param second the second
     * @return the result
     */
        public static  Function<Term, Boolean> apply(Term second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualTerm operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static  Boolean apply(Term first, Term second) {
        return first.equals(second);
    }
}
