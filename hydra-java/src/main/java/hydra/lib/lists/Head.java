package hydra.lib.lists;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.List;

public class Head<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.head");
    }

   public static <X> X apply(List<X> list) {
        return list.get(0);
    }
}
