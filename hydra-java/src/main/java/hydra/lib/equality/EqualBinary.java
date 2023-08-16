package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualBinary<A> extends EqualityFunction<A, String> {
    public EqualBinary() {
        super(PrimitiveType.binary());
    }

    public static Function<String, Boolean> apply(String second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(String first, String second) {
        return 0 == first.compareTo(second);
    }
}
