package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.Set;
import static hydra.dsl.Types.*;

public class Singleton<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.singleton");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", set("x")));
    }

    public static <X> Set<X> apply(X elem) {
        Set<X> newSet = new HashSet();
        newSet.add(elem);
        return newSet;
    }
}
