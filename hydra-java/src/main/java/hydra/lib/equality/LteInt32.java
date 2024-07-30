package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class LteInt32 extends EqualityFunction<Integer> {
    public LteInt32() {
        super(PrimitiveType.int32(), Relation.LESS_THAN_OR_EQUAL);
    }

    public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Integer first, Integer second) {
        return first.compareTo(second) <= 0;
    }
}
