package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.math.BigInteger;
import java.util.function.Function;


public class EqualBigint<A> extends EqualityFunction<A, BigInteger> {
    public EqualBigint() {
        super(PrimitiveType.bigint());
    }

    public static Function<BigInteger, Boolean> apply(BigInteger second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(BigInteger first, BigInteger second) {
        return 0 == first.compareTo(second);
    }
}
