package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.equality.EqualTerm;
import hydra.lib.equality.EqualType;


public interface Equality {
    static <A> Term<A> equalTerm() {
        return new EqualTerm<A>().term();
    }

    static <A> Term<A> equalType() {
        return new EqualType<A>().term();
    }
}
