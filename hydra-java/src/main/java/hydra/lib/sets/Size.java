package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Set;

public class Size<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.size");
    }

    public static <A> Integer apply(Set<A> arg) {
        return arg.size();
    }
}
