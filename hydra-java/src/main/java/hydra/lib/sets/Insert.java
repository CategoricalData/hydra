package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.HashSet;
import java.util.Set;

public class Insert<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.insert");
    }

    public static <X> Function<Set<X>, Set<X>> apply(X elem) {
        return (arg) -> {
            Set<X> newSet = new HashSet(arg);
            newSet.add(elem);
            return newSet;
        };
    }

    public static <X> Set<X> apply(X elem, Set<X> arg) {
        return Insert.apply(elem).apply(arg);
    }
}
