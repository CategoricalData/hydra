package hydra.lib.lists;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.List;

public class Last<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.last");
    }

    public static <X> X apply(List<X> list) {
        return list.get(list.size() - 1);
    }
}
