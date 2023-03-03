package hydra.lib.optionals;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Optional;

public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.pure");
    }

    public static <X> Optional<X> pure(X arg) {
        return Optional.of(arg);
    }
}
