package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import static hydra.dsl.Types.*;

public class Sub<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.sub");
    }

    @Override
    public Type<A> type() {
        return function(int32(), int32(), int32());
    }

    public static Function<Integer, Integer> apply(Integer minuend) {
        return (subtrahend) -> apply(minuend, subtrahend);
    }

    public static Integer apply(Integer minuend, Integer subtrahend) {
        return (minuend - subtrahend);
    }
}
