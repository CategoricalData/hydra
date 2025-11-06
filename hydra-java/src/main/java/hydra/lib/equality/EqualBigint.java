package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.math.BigInteger;
import java.util.function.Function;


/**
 * Tests equality of two arbitrary-precision integers.
 */
public class EqualBigint extends EqualityFunction<BigInteger> {
    public EqualBigint() {
        super(PrimitiveType.bigint(), Relation.EQUALS);
    }

    /**
     * Applies the EqualBigint operation.
     * @param second the second
     * @return the result
     */
        public static Function<BigInteger, Boolean> apply(BigInteger second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualBigint operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(BigInteger first, BigInteger second) {
        return first.equals(second);
    }
}
