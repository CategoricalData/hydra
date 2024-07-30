package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualInt16 extends EqualityFunction<Short> {
    public EqualInt16() {
        super(PrimitiveType.int16(), Relation.EQUALS);
    }

    public static Function<Short, Boolean> apply(Short second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Short first, Short second) {
        return first.equals(second);
    }
}
