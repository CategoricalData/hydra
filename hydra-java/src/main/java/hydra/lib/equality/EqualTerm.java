package hydra.lib.equality;

import hydra.core.Term;
import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualTerm extends EqualityFunction<Term> {
    public EqualTerm() {
        super(PrimitiveType.term(), Relation.EQUALS);
    }

    public static  Function<Term, Boolean> apply(Term second) {
        return first -> apply(first, second);
    }

    public static  Boolean apply(Term first, Term second) {
        return first.equals(second);
    }
}
