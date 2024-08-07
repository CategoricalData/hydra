package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.math.BigInteger;
import java.util.function.Function;


public class EqualBigint extends EqualityFunction<BigInteger> {
    public EqualBigint() {
        super(PrimitiveType.bigint(), Relation.EQUALS);
    }

    public static Function<BigInteger, Boolean> apply(BigInteger second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(BigInteger first, BigInteger second) {
        return first.equals(second);
    }
}
