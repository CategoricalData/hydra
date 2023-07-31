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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;

public class Concat2<A> extends PrimitiveFunction<A> {
    public static final Name NAME = new Name("hydra/lib/lists.concat2");

    public Name name() {
        return NAME;
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), list("x"), list("x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map2(
                Expect.list(Flows::pure, args.get(0)),
                Expect.list(Flows::pure, args.get(1)),
                (l1, l2) -> Terms.list(Concat2.apply(l1, l2)));
    }

    public static <X> List<X> apply(List<X> l1, List<X> l2) {
        List<X> combined = new ArrayList<>();
        combined.addAll(l1);
        combined.addAll(l2);
        return combined;
    }
}
