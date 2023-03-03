package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class Add<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.add");
    }

    public static Integer apply(Integer augend, Integer addend) {
        return (augend + addend);
    }
}
