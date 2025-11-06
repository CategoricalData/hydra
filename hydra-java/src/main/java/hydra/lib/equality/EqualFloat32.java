package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 32-bit floating-point numbers.
 */
public class EqualFloat32 extends EqualityFunction<Float> {
    public EqualFloat32() {
        super(PrimitiveType.float32(), Relation.EQUALS);
    }

    /**
     * Applies the EqualFloat32 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Float, Boolean> apply(Float second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualFloat32 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Float first, Float second) {
        return first.equals(second);
    }
}
