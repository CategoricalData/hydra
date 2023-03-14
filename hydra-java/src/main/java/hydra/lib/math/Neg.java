package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;
import static hydra.dsl.Types.*;

public class Neg<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.neg");
    }

    @Override
    public Type<A> type() {
        return function(int32(), int32());
    }

    public static Integer apply(Integer num) {
        return (-1 * num);
    }
}
