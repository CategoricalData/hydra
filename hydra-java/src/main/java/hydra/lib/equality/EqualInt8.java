package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 8-bit signed integers.
 */
public class EqualInt8 extends EqualityFunction<Byte> {
    public EqualInt8() {
        super(PrimitiveType.int8(), Relation.EQUALS);
    }

    /**
     * Applies the EqualInt8 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Byte, Boolean> apply(Byte second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualInt8 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Byte first, Byte second) {
        return first.equals(second);
    }
}
