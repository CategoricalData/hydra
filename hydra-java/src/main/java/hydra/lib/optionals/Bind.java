package hydra.lib.optionals;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;
import static hydra.dsl.Types.*;

public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.bind");
    }

    @Override
    public Type<A> type() {
        return lambda("x", lambda("y", function(optional("x"), function("x", optional("y")), optional("y"))));
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
