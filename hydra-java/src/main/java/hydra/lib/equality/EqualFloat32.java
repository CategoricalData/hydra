package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualFloat32 extends EqualityFunction<Float> {
    public EqualFloat32() {
        super(PrimitiveType.float32(), Relation.EQUALS);
    }

    public static Function<Float, Boolean> apply(Float second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Float first, Float second) {
        return first.equals(second);
    }
}
