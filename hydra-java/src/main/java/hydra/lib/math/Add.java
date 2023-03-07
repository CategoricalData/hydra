package hydra.lib.math;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.function.Function;

public class Add<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.add");
    }

    public static Function<Integer, Integer> apply(Integer augend) {
        return (addend) -> augend + addend;
    }

    public static Integer apply(Integer augend, Integer addend) {
        return Add.apply(augend).apply(addend);
    }
}
