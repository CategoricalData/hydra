package hydra.lib.strings;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.List;

public class Cat<M> extends PrimitiveFunction<M> {
    public Name name() {
        return new Name("hydra/lib/strings.cat");
    }

    public static String apply(List<String> args) {
        return String.join("", args);
    }
}
