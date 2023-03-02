package hydra.lib.optionals;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;

public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.bind");
    }

    public static <B, C> Optional<C> apply(Optional<B> optionalArg, Function<B, Optional<C>> f) {
        if (!optionalArg.isPresent()) {
            return Optional.empty();
        }

        B arg = optionalArg.get();
        
        return f.apply(arg);
    }
}
