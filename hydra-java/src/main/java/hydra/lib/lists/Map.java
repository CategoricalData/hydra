package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.function.Function;
import java.util.List;
import java.util.stream.Collectors;

import static hydra.Flows.*;
import static hydra.dsl.Types.*;

public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.map");
    }

    @Override
    public Type<A> type() {
        return lambda("a", lambda("b", function(function("a", "b"), list("a"), list("b"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> bind(Expect.list(instance -> pure(Terms.apply(args.get(0), instance)), args.get(1)),
            l -> pure(Terms.list(l)));
    }

    public static <X, Y> Function<List<X>, List<Y>> apply(Function<X, Y> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    public static <X, Y> List<Y> apply(Function<X, Y> mapping, List<X> arg) {
        return arg.stream().map(mapping).collect(Collectors.toList());
    }
}
