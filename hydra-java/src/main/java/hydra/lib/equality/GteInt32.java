package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class GteInt32<A> extends EqualityFunction<A, Integer> {
    public GteInt32() {
        super(PrimitiveType.int32(), Relation.GREATER_THAN_OR_EQUAL);
    }

    public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Integer first, Integer second) {
        return first.compareTo(second) >= 0;
    }
}
