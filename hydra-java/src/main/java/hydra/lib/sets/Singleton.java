package hydra.lib.sets;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.HashSet;
import java.util.Set;

public class Singleton<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.singleton");
    }

    public static <A> Set<A> apply(A elem) {
        Set<A> newSet = new HashSet();
        newSet.add(elem);
        return newSet;
    }
}
