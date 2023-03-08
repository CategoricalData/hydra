package hydra.lib.lists;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.List;
import java.util.stream.Collectors;

public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.bind");
    }

    public static <X, Y> Function<Function<X, List<Y>>, List<Y>> apply(List<X> args) {
        return (mapping) -> apply(args, mapping);
    }

    public static <X, Y> List<Y> apply(List<X> args, Function<X, List<Y>> mapping) {
        return args.stream().flatMap(x -> mapping.apply(x).stream()).collect(Collectors.toList());
    }
}
