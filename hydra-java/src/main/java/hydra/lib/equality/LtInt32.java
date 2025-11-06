package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests if the first 32-bit signed integer is less than the second.
 */
public class LtInt32 extends EqualityFunction<Integer> {
    public LtInt32() {
        super(PrimitiveType.int32(), Relation.LESS_THAN);
    }

    /**
     * Applies the LtInt32 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the LtInt32 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Integer first, Integer second) {
        return first.compareTo(second) < 0;
    }
}
