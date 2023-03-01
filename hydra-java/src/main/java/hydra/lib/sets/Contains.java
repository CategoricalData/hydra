package hydra.lib.sets;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.Set;

public class Contains<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.contains");
    }

    public static <A> Boolean apply(A elem, Set<A> arg) {
        return arg.contains(elem);
    }
}
