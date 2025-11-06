package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 16-bit signed integers.
 */
public class EqualInt16 extends EqualityFunction<Short> {
    public EqualInt16() {
        super(PrimitiveType.int16(), Relation.EQUALS);
    }

    /**
     * Applies the EqualInt16 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Short, Boolean> apply(Short second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualInt16 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Short first, Short second) {
        return first.equals(second);
    }
}
