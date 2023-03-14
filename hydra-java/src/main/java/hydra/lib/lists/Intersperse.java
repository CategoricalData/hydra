package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.function.Function;
import java.util.List;
import static hydra.dsl.Types.*;

public class Intersperse<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.intersperse");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", list("x"), list("x")));
    }

    public static <X> Function<List<X>, List<X>> apply(X delim) {
        return (list) -> apply(delim, list);
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
