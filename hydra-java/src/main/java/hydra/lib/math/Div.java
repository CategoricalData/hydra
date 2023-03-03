package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class Div<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.div");
    }

    public static Integer apply(Integer dividend, Integer divisor) {
        return (dividend / divisor);
    }
}
