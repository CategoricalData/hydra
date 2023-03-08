package hydra.lib.lists;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.ArrayList;
import java.util.function.Function;
import java.util.List;

public class Intercalate<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.intercalate");
    }

    public static <X> Function<List<List<X>>, List<X>> apply(List<X> delim) {
        return (sublists) -> apply(delim, sublists);
    }

    public static <X> List<X> apply(List<X> delim, List<List<X>> sublists) {
        List<X> result = new ArrayList<>();
        boolean first = true;
        for (List<X> sublist : sublists) {
            if (first) {
                first = false;
            } else {
                result.addAll(delim);
            }
            result.addAll(sublist);
        }
        return result;
    }
}
