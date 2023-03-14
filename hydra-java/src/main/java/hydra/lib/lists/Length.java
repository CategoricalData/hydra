package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import static hydra.dsl.Types.*;

public class Length<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.length");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), int32()));
    }

    public static <X> int apply(List<X> list) {
        return list.size();
    }
}
