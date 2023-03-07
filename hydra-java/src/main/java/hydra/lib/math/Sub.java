package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;

public class Sub<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.sub");
    }

    public static Function<Integer, Integer> apply(Integer minuend) {
        return (subtrahend) -> minuend - subtrahend;
    }

    public static Integer apply(Integer minuend, Integer subtrahend) {
        return Sub.apply(minuend).apply(subtrahend);
    }
}
