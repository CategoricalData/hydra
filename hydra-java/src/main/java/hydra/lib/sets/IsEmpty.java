package hydra.lib.sets;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.Set;

public class IsEmpty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.isEmpty");
    }

    public static <A> Boolean apply(Set<A> arg) {
        return arg.isEmpty();
    }
}
