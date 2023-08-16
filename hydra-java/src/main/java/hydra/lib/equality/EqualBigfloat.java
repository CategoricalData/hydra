package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualBigfloat<A> extends EqualityFunction<A, Double> {
    public EqualBigfloat() {
        super(PrimitiveType.bigfloat());
    }

    public static Function<Double, Boolean> apply(Double second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Double first, Double second) {
        return 0 == first.compareTo(second);
    }
}
