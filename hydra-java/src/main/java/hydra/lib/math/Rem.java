package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;

public class Rem<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.rem");
    }

    public static Function<Integer, Integer> apply(Integer dividend) {
        // % in Java is a mathematical remainder, not modulus
        return (divisor) -> dividend % divisor;
    }

    public static Integer apply(Integer dividend, Integer divisor) {
        return Rem.apply(dividend).apply(divisor);
    }
}
