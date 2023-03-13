package hydra.lib.sets;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.Set;

public class Singleton<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.singleton");
    }

    public static <X> Set<X> apply(X elem) {
        Set<X> newSet = new HashSet();
        newSet.add(elem);
        return newSet;
    }
}
