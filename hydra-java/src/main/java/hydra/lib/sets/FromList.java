package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import static hydra.dsl.Types.*;

public class FromList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.fromList");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), set("x")));
    }

    public static <X> Set<X> apply(List<X> arg) {
        return new HashSet(arg);
    }
}
