package hydra.lib.strings;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

public class ToUpper<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.toUpper");
    }

    public static String apply(String lower) {
        return lower.toUpperCase(); // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
    }
}
