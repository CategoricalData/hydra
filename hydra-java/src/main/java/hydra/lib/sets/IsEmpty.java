package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Set;

public class IsEmpty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.isEmpty");
    }

    public static <B> Boolean apply(Set<B> arg) {
        return arg.isEmpty();
    }
}
