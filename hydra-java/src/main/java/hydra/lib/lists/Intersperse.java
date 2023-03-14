package hydra.lib.lists;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
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

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(
            Expect.list(Flows::pure, args.get(1)),
            list -> Terms.list(apply(args.get(0), list)));
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
