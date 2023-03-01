package hydra.lib.strings;

import hydra.core.Name;
import hydra.PrimitiveFunction;

public class Length<M> extends PrimitiveFunction<M> {
    public Name name() {
        return new Name("hydra/lib/strings.length");
    }

    public static int apply(String s) {
        return s.length();
    }
}
