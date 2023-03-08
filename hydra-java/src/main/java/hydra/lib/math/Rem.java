package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class Rem<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.rem");
    }

    public static Integer apply(Integer dividend, Integer divisor) {
        // % in Java is a mathematical remainder, not modulus
        return (dividend % divisor);
    }
}
