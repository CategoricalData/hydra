package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualInt64<A> extends EqualityFunction<A, Long> {
    public EqualInt64() {
        super(PrimitiveType.int64(), Relation.EQUALS);
    }

    public static Function<Long, Boolean> apply(Long second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Long first, Long second) {
        return first.equals(second);
    }
}
