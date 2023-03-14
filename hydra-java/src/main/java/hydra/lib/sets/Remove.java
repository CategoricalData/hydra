package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.HashSet;
import java.util.Set;
import static hydra.dsl.Types.*;

public class Remove<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.remove");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", set("x")));
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
