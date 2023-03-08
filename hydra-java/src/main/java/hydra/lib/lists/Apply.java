package hydra.lib.lists;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.List;
import java.util.stream.Collectors;


public class Apply<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.apply");
    }

    public static <X, Y> Function<List<X>, List<Y>> apply(List<Function<X, Y>> functions) {
        return (args) -> apply(functions, args);
    }

    public static <X, Y> List<Y> apply(List<Function<X, Y>> functions, List<X> args) {
        return functions.stream().flatMap(f -> args.stream().map(f)).collect(Collectors.toList());
    }
}
