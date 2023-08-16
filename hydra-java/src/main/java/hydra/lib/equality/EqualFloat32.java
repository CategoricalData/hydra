package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualFloat32<A> extends EqualityFunction<A, Float> {
    public EqualFloat32() {
        super(PrimitiveType.float32());
    }
    public static Function<Float, Boolean> apply(Float second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Float first, Float second) {
        return 0 == first.compareTo(second);
    }
}
