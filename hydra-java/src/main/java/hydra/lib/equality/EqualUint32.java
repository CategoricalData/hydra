package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualUint32 extends EqualityFunction<Long> {
    public EqualUint32() {
        super(PrimitiveType.uint32(), Relation.EQUALS);
    }

    public static Function<Long, Boolean> apply(Long second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Long first, Long second) {
        return first.equals(second);
    }
}
