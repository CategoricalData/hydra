package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualString<A> extends EqualityFunction<A, String> {
    public EqualString() {
        super(PrimitiveType.string());
    }

    public static Function<String, Boolean> apply(String second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(String first, String second) {
        return 0 == first.compareTo(second);
    }
}
