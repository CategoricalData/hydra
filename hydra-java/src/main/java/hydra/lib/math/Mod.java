package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import static hydra.dsl.Types.*;

public class Mod<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.mod");
    }

    @Override
    public Type<A> type() {
        return function(int32(), int32(), int32());
    }

    public static Function<Integer, Integer> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    public static Integer apply(Integer dividend, Integer divisor) {
        return java.lang.Math.floorMod(dividend, divisor);
    }
}
