package hydra.lib.optionals;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Optional;

public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.pure");
    }

    public static <B> Optional<B> pure(B arg) {
        return Optional.of(arg);
    }
}
