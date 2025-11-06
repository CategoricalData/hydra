package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 32-bit signed integers.
 */
public class EqualInt32 extends EqualityFunction<Integer> {
    public EqualInt32() {
        super(PrimitiveType.int32(), Relation.EQUALS);
    }

    /**
     * Applies the EqualInt32 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualInt32 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Integer first, Integer second) {
        return first.equals(second);
    }
}
