package hydra.lib.equality;

import hydra.core.Term;
import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualTerm<A> extends EqualityFunction<A, Term<A>> {
    public EqualTerm() {
        super(PrimitiveType.term());
    }

    public static <A> Function<Term<A>, Boolean> apply(Term<A> second) {
        return first -> apply(first, second);
    }

    public static <A> Boolean apply(Term<A> first, Term<A> second) {
        return first.equals(second);
    }
}
