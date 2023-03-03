package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.HashSet;
import java.util.Set;

public class Remove<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.remove");
    }

    public static <X> Set<X> apply(X elem, Set<X> arg) {
        Set<X> newSet = new HashSet(arg);
        newSet.remove(elem);
        return newSet;
    }
}
