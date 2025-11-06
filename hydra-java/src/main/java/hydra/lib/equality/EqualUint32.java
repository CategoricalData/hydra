package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 32-bit unsigned integers.
 */
public class EqualUint32 extends EqualityFunction<Long> {
    public EqualUint32() {
        super(PrimitiveType.uint32(), Relation.EQUALS);
    }

    /**
     * Applies the EqualUint32 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Long, Boolean> apply(Long second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualUint32 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Long first, Long second) {
        return first.equals(second);
    }
}
