package hydra.lib.lists;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.List;

public class Length<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.length");
    }

    public static <X> int apply(List<X> list) {
        return list.size();
    }
}
