package hydra.lib.sets;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.function.Function;
import java.util.Set;
import java.util.stream.Collectors;

public class Map<A, B> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.map");
    }

    public static <A, B> Set<B> apply(Function<A, B> mapping, Set<A> arg) {
        return arg.stream().map(mapping).collect(Collectors.toSet());
    }
}
