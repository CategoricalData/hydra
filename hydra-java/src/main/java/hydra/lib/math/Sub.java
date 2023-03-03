package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

public class Sub<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.sub");
    }

    public static Integer add(Integer minuend, Integer subtrahend) {
        return (minuend - subtrahend);
    }
}
