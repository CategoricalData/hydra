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

import static hydra.dsl.Types.*;

public class IsEmpty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.isEmpty");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(set("x"), boolean_()));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(0)), arg -> Terms.boolean_(apply(arg)));
    }

    public static <X> Boolean apply(Set<X> arg) {
        return arg.isEmpty();
    }
}
