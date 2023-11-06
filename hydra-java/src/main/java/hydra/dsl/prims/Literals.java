package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.literals.ShowInt32;
import hydra.lib.literals.ShowString;


public interface Literals {
    static <A> Term<A> showInt32() {
        return new ShowInt32<A>().term();
    }

    static <A> Term<A> showString() {
        return new ShowString<A>().term();
    }
}
