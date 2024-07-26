package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualBigfloat extends EqualityFunction<Double> {
    public EqualBigfloat() {
        super(PrimitiveType.bigfloat(), Relation.EQUALS);
    }

    public static Function<Double, Boolean> apply(Double second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Double first, Double second) {
        return first.equals(second);
    }
}
