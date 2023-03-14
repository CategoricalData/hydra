package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import static hydra.dsl.Types.*;

public class ToList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.toList");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(set("x"), list("x")));
    }

    public static <X> List<X> apply(Set<X> arg) {
        return new ArrayList(arg);
    }
}
