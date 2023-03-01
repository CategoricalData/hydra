package hydra.lib.strings;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class ToLower<M> extends PrimitiveFunction<M> {
    public Name name() {
        return new Name("hydra/lib/strings.toLower");
    }

    public static String apply(String upper) {
        return upper.toLowerCase(); // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
    }
}
