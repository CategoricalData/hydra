package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests if the first 32-bit signed integer is less than or equal to the second.
 */
public class LteInt32 extends EqualityFunction<Integer> {
    public LteInt32() {
        super(PrimitiveType.int32(), Relation.LESS_THAN_OR_EQUAL);
    }

    /**
     * Applies the LteInt32 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the LteInt32 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Integer first, Integer second) {
        return first.compareTo(second) <= 0;
    }
}
