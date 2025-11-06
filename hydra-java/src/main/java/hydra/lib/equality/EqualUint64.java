package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.math.BigInteger;
import java.util.function.Function;


/**
 * Tests equality of two 64-bit unsigned integers.
 */
public class EqualUint64 extends EqualityFunction<BigInteger> {
    public EqualUint64() {
        super(PrimitiveType.uint64(), Relation.EQUALS);
    }

    /**
     * Applies the EqualUint64 operation.
     * @param second the second
     * @return the result
     */
        public static Function<BigInteger, Boolean> apply(BigInteger second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualUint64 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(BigInteger first, BigInteger second) {
        return first.equals(second);
    }
}
