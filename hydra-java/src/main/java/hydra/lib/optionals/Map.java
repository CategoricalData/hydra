package hydra.lib.optionals;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;

public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.map");
    }

    public static <B, C> Optional<C> map(Function<B, C> f, Optional<B> optionalArg) {
        if (!optionalArg.isPresent()) {
            return Optional.empty();
        }

        B arg = optionalArg.get();

        return Optional.of(f.apply(arg));
    }
}
