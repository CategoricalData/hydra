package hydra.lib.optionals;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;
import java.util.Optional;

public class Apply<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.apply");
    }

    public static <B, C> Optional<C> apply(Optional<Function<B, C>> optionalF, Optional<B> optionalArg) {
        if (!optionalF.isPresent() || !optionalArg.isPresent()) {
            return Optional.empty();
        }
    
        Function<B, C> f = optionalF.get();
        B arg = optionalArg.get();

        return Optional.of(f.apply(arg));
    }
}
