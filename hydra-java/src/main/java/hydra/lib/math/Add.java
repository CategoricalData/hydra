package hydra.lib.math;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;

public class Add<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.add");
    }

    public static Function<Integer, Integer> apply(Integer augend) {
        return (addend) -> apply(augend, addend);
    }

    public static Integer apply(Integer augend, Integer addend) {
        return (augend + addend);
    }
}
