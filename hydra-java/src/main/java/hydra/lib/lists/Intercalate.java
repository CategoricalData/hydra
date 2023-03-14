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
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class Intercalate<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.intercalate");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), list(list("x")), list("x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map2(
            Expect.list(Flows::pure, args.get(0)),
            Expect.list(t -> Expect.list(Flows::pure, t), args.get(1)),
            (delim1, sublists1) -> Terms.list(apply(delim1, sublists1)));
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
