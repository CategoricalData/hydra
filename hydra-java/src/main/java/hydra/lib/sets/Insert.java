package hydra.lib.sets;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.HashSet;
import java.util.Set;

public class Insert<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.insert");
    }

    public static <A> Set<A> apply(A elem, Set<A> arg) {
        Set<A> newSet = new HashSet(arg);
        newSet.add(elem);
        return newSet;
    }
}
