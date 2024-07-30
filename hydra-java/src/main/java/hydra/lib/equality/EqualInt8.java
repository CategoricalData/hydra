package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualInt8 extends EqualityFunction<Short> {
    public EqualInt8() {
        super(PrimitiveType.int8(), Relation.EQUALS);
    }

    public static Function<Short, Boolean> apply(Short second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Short first, Short second) {
        return first.equals(second);
    }
}
