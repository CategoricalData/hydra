package hydra.lib.math;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

public class Neg<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.neg");
    }

    public static Integer apply(Integer num) {
        return (-1 * num);
    }
}
