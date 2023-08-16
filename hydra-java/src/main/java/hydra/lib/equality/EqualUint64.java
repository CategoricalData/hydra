package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.math.BigInteger;
import java.util.function.Function;


public class EqualUint64<A> extends EqualityFunction<A, BigInteger> {
    public EqualUint64() {
        super(PrimitiveType.uint64(), Relation.EQUALS);
    }

    public static Function<BigInteger, Boolean> apply(BigInteger second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(BigInteger first, BigInteger second) {
        return first.equals(second);
    }
}
