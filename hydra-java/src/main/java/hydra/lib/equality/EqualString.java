package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualString extends EqualityFunction<String> {
    public EqualString() {
        super(PrimitiveType.string(), Relation.EQUALS);
    }

    public static Function<String, Boolean> apply(String second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(String first, String second) {
        return first.equals(second);
    }
}
