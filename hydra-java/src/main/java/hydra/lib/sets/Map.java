package hydra.lib.sets;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.Set;
import java.util.stream.Collectors;

public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.map");
    }

    public static <X, Y> Set<Y> apply(Function<X, Y> mapping, Set<X> arg) {
        return arg.stream().map(mapping).collect(Collectors.toSet());
    }
}
