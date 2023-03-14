package hydra.lib.optionals;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.Optional;
import static hydra.dsl.Types.*;

public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.pure");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", optional("x")));
    }

    public static <X> Optional<X> pure(X arg) {
        return Optional.of(arg);
    }
}
