package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Set;

public class IsEmpty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.isEmpty");
    }

    public static <X> Boolean apply(Set<X> arg) {
        return arg.isEmpty();
    }
}
