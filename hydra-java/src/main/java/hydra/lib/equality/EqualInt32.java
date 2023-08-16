package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualInt32<A> extends EqualityFunction<A, Integer> {
    public EqualInt32() {
        super(PrimitiveType.int32());
    }

    public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Integer first, Integer second) {
        return 0 == first.compareTo(second);
    }
}
