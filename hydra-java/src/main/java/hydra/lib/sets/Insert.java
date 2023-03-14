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
import java.util.function.Function;
import java.util.HashSet;
import java.util.Set;
import static hydra.dsl.Types.*;

public class Insert<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.insert");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", set("x"), set("x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(1)), arg -> Terms.set(apply(args.get(0), arg)));
    }

    public static <X> Function<Set<X>, Set<X>> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    public static <X> Set<X> apply(X elem, Set<X> arg) {
        Set<X> newSet = new HashSet<>(arg);
        newSet.add(elem);
        return newSet;
    }
}
