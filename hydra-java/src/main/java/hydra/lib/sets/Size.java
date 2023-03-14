package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.Set;
import static hydra.dsl.Types.*;

public class Size<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.size");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(set("x"), int32()));
    }

    public static <X> Integer apply(Set<X> arg) {
        return arg.size();
    }
}
