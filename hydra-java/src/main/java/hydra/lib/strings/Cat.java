package hydra.lib.strings;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.List;

public class Cat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.cat");
    }

    public static String apply(List<String> args) {
        return String.join("", args);
    }
}
