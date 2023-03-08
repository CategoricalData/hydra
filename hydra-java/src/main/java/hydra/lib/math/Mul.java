package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class Mul<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.mul");
    }

    public static Integer apply(Integer multiplier, Integer multiplicand) {
        return (multiplier * multiplicand);
    }
}
