package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.literals.*;

public interface Literals {
    static <A> Term<A> showInt32() {
        return new ShowInt32().term();
    }

    static <A> Term<A> showString() {
        return new ShowString().term();
    }
}
