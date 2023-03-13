package hydra.lib.optionals;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;

public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.bind");
    }

    public static <X, Y> Function<Function<X, Optional<Y>>, Optional<Y>> apply(Optional<X> optionalArg) {
        return (f) -> apply(optionalArg, f);
    }

    public static <X, Y> Optional<Y> apply(Optional<X> optionalArg, Function<X, Optional<Y>> f) {
        if (!optionalArg.isPresent()) {
            return Optional.empty();
        }

        X arg = optionalArg.get();

        return f.apply(arg);
    }
}
