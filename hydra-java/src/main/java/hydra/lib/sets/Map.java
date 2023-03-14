package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.Set;
import java.util.stream.Collectors;
import static hydra.dsl.Types.*;

public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.map");
    }

    @Override
    public Type<A> type() {
        return lambda("x", lambda("y", function(function("x", "y"), set("x"), set("y"))));
    }

    public static <X, Y> Function<Set<X>, Set<Y>> apply(Function<X, Y> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    public static <X, Y> Set<Y> apply(Function<X, Y> mapping, Set<X> arg) {
        return arg.stream().map(mapping).collect(Collectors.toSet());
    }
}
