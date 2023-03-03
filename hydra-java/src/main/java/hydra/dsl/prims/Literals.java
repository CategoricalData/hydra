package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.literals.*;

public interface Literals {
    static <M> Term<M> showInt32() {
        return new ShowInt32().term();
    }

    static <M> Term<M> showString() {
        return new ShowString().term();
    }
}
