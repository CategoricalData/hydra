package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.function.Function;
import java.util.List;
import static hydra.dsl.Types.*;

public class Intercalate<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.intercalate");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), list(list("x")), list("x")));
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
