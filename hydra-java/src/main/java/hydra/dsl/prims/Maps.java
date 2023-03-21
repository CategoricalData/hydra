package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.maps.Empty;


public interface Maps {
    static <A> Term<A> add() {
        return new Empty<A>().term();
    }
}
