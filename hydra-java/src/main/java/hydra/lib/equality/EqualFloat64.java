package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 64-bit floating-point numbers.
 */
public class EqualFloat64 extends EqualityFunction<Double> {
    public EqualFloat64() {
        super(PrimitiveType.float64(), Relation.EQUALS);
    }

    /**
     * Applies the EqualFloat64 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Double, Boolean> apply(Double second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualFloat64 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Double first, Double second) {
        return first.equals(second);
    }
}
