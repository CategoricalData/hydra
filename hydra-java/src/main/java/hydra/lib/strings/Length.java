package hydra.lib.strings;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class Length<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.length");
    }

    public static int apply(String s) {
        return s.length();
    }
}
