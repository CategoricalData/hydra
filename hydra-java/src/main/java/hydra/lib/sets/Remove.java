package hydra.lib.sets;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.HashSet;
import java.util.Set;

public class Remove<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.remove");
    }

    public static <X> Function<Set<X>, Set<X>> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    public static <X> Set<X> apply(X elem, Set<X> arg) {
        Set<X> newSet = new HashSet(arg);
        newSet.remove(elem);
        return newSet;
    }
}
