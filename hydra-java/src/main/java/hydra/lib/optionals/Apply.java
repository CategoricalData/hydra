package hydra.lib.optionals;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;

public class Apply<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.apply");
    }

    public static <X, Y> Function<Optional<X>, Optional<Y>> apply(Optional<Function<X, Y>> optionalF) {
        return (optionalArg) -> apply(optionalF, optionalArg);
    }

    public static <X, Y> Optional<Y> apply(Optional<Function<X, Y>> optionalF, Optional<X> optionalArg) {
        if (!optionalF.isPresent() || !optionalArg.isPresent()) {
            return Optional.empty();
        }
    
        Function<X, Y> f = optionalF.get();
        X arg = optionalArg.get();

        return Optional.of(f.apply(arg));
    }
}
