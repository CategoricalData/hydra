package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import static hydra.dsl.Types.*;

public class Head<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.head");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), "x"));
    }

    public static <X> X apply(List<X> list) {
        return list.get(0);
    }
}
