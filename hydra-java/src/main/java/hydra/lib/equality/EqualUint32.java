package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualUint32<A> extends EqualityFunction<A, Long> {
    public EqualUint32() {
        super(PrimitiveType.uint32());
    }

    public static Function<Long, Boolean> apply(Long second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Long first, Long second) {
        return 0 == first.compareTo(second);
    }
}
