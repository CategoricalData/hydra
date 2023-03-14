package hydra.lib.optionals;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;
import static hydra.dsl.Types.*;

public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.map");
    }

    @Override
    public Type<A> type() {
        return lambda("x", lambda("y", function(function("x", "y"), optional("x"), optional("y"))));
    }

    public static <X, Y> Function<Optional<X>, Optional<Y>> apply(Function<X, Y> f) {
        return (optionalArg) -> apply(f, optionalArg);
    }

    public static <X, Y> Optional<Y> apply(Function<X, Y> f, Optional<X> optionalArg) {
        if (!optionalArg.isPresent()) {
            return Optional.empty();
        }

        X arg = optionalArg.get();

        return Optional.of(f.apply(arg));
    }
}
