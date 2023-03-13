package hydra.lib.sets;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.Set;

public class Contains<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.contains");
    }

    public static <X> Function<Set<X>, Boolean> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    public static <X> Boolean apply(X elem, Set<X> arg) {
        return arg.contains(elem);
    }
}
