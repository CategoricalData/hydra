package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import static hydra.dsl.Types.*;


public class ToUpper<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.toUpper");
    }

    @Override
    public Type<A> type() {
        return function(string(), string());
    }

    public static String apply(String lower) {
        return lower.toUpperCase(); // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
    }
}
