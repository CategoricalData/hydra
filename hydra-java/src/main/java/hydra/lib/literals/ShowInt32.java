package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;
import static hydra.dsl.Types.*;

public class ShowInt32<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.showInt32");
    }

    @Override
    public Type<A> type() {
        return function(int32(), string());
    }

    public static String apply(Integer value) {
        return Integer.toString(value);
    }
}
