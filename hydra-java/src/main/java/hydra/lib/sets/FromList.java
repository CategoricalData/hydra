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

    public static <B> Set<B> apply(List<B> arg) {
        return new HashSet(arg);
    }
}
