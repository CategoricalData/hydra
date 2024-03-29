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

import java.util.List;
import java.util.function.Function;

import static hydra.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;

public class Null<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.null");
    }

    @Override
    public Type<A> type() {
        return lambda("a", function(list("a"), int32()));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map(Expect.list(Flows::pure, args.get(0)), l -> Terms.boolean_(apply(l)));
    }

    public static <X> boolean apply(List<X> list) {
        return list.isEmpty();
    }
}
