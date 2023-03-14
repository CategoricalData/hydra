package hydra.lib.sets;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.*;

public class Singleton<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/sets.singleton");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", set("x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.pure(Terms.set(apply(args.get(0))));
    }

    public static <X> Set<X> apply(X elem) {
        Set<X> newSet = new HashSet<>();
        newSet.add(elem);
        return newSet;
    }
}
