package hydra.lib.strings;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class ToUpper<M> extends PrimitiveFunction<M> {
    public Name name() {
        return new Name("hydra/lib/strings.toUpper");
    }

    public static String apply(String lower) {
        return lower.toUpperCase(); // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
    }
}
