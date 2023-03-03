package hydra.lib.lists;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;

public class Intersperse<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.intersperse");
    }

    public static <X> List<X> apply(X delim, List<X> list) {
        List<X> result = new ArrayList<>();
        boolean first = true;
        for (X a : list) {
            if (first) {
                first = false;
            } else {
                result.add(delim);
            }
            result.add(a);
        }
        return result;
    }
}
