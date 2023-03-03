package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ToList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.toList");
    }

    public static <X> List<X> apply(Set<X> arg) {
        return new ArrayList(arg);
    }
}
