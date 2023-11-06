package hydra.lib.sets;

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
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.set;


public class Size<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.size");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(set("x"), int32()));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(0)), arg -> Terms.int32(apply(arg)));
    }

    public static <X> Integer apply(Set<X> arg) {
        return arg.size();
    }
}
