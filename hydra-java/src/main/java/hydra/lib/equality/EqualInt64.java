package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 64-bit signed integers.
 */
public class EqualInt64 extends EqualityFunction<Long> {
    public EqualInt64() {
        super(PrimitiveType.int64(), Relation.EQUALS);
    }

    /**
     * Applies the EqualInt64 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Long, Boolean> apply(Long second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualInt64 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Long first, Long second) {
        return first.equals(second);
    }
}
