package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Set;

public class Contains<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.contains");
    }

    public static <X> Boolean apply(X elem, Set<X> arg) {
        return arg.contains(elem);
    }
}
