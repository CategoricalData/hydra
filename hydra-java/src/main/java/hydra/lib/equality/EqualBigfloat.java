package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.math.BigDecimal;
import java.util.function.Function;


/**
 * Tests equality of two arbitrary-precision floating-point numbers.
 */
public class EqualBigfloat extends EqualityFunction<BigDecimal> {
    public EqualBigfloat() {
        super(PrimitiveType.bigfloat(), Relation.EQUALS);
    }

    /**
     * Applies the EqualBigfloat operation.
     * @param second the second
     * @return the result
     */
        public static Function<BigDecimal, Boolean> apply(BigDecimal second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualBigfloat operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(BigDecimal first, BigDecimal second) {
        return first.equals(second);
    }
}
