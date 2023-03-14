package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.Set;
import static hydra.dsl.Types.*;

public class IsEmpty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.isEmpty");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(set("x"), boolean_()));
    }

    public static <X> Boolean apply(Set<X> arg) {
        return arg.isEmpty();
    }
}
