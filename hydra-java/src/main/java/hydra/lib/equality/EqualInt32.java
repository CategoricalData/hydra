package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualInt32 extends EqualityFunction<Integer> {
    public EqualInt32() {
        super(PrimitiveType.int32(), Relation.EQUALS);
    }

    public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Integer first, Integer second) {
        return first.equals(second);
    }
}
