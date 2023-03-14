package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.Set;
import static hydra.dsl.Types.*;

public class Empty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.empty");
    }

    @Override
    public Type<A> type() {
        return lambda("x", set("x"));
    }

    public static <X> Set<X> apply() {
        return Collections.emptySet();
    }
}
