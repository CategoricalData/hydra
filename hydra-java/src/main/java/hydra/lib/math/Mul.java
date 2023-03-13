package hydra.lib.math;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;

public class Mul<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.mul");
    }

    public static Function<Integer, Integer> apply(Integer multiplier) {
        return (multiplicand) -> apply(multiplier, multiplicand);
    }

    public static Integer apply(Integer multiplier, Integer multiplicand) {
        return (multiplier * multiplicand);
    }
}
