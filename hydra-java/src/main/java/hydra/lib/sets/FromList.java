package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class FromList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.fromList");
    }

    public static <X> Set<X> apply(List<X> arg) {
        return new HashSet(arg);
    }
}
