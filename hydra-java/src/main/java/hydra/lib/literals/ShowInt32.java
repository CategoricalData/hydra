package hydra.lib.literals;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

public class ShowInt32<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.showInt32");
    }

    public static String apply(Integer value) {
        return Integer.toString(value);
    }
}
