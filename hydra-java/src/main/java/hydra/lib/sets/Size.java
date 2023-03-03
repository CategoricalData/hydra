package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Set;

public class Size<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.size");
    }

    public static <X> Integer apply(Set<X> arg) {
        return arg.size();
    }
}
