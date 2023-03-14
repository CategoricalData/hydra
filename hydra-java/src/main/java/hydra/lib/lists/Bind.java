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
import java.util.stream.Collectors;

import static hydra.dsl.Types.*;


public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.bind");
    }

    @Override
    public Type<A> type() {
        return lambda("x", lambda("y", function(list("x"), function("x", list("y")), list("y"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map2(Expect.list(Flows::pure, args.get(0)), Expect.list(Flows::pure, args.get(1)),
            (argsArg, mappingsArg) -> Terms.list(argsArg.stream()
                .flatMap(arg -> mappingsArg.stream().map(fun -> Terms.apply(fun, arg)))
                .collect(Collectors.toList())));
    }

    public static <X, Y> Function<Function<X, List<Y>>, List<Y>> apply(List<X> args) {
        return (mapping) -> apply(args, mapping);
    }

    public static <X, Y> List<Y> apply(List<X> args, Function<X, List<Y>> mapping) {
        return args.stream().flatMap(x -> mapping.apply(x).stream()).collect(Collectors.toList());
    }
}
