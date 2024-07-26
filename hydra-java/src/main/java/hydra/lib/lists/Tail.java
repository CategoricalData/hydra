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
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;

public class Tail extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra/lib/lists.tail");
    }

    @Override
    public Type type() {
        return lambda("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.list(Flows::pure, args.get(0)),
                (Function<List<Term>, Term>) terms -> Terms.list(apply(terms)));
    }

    public static <X> List<X> apply(List<X> list) {
        return list.subList(1, list.size());
    }
}
