package hydra.lib.flows;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.lib.lists.Cons;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Terms.app;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.list;
import static hydra.dsl.Terms.project;
import static hydra.dsl.Terms.variable;


public class MapList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/flows.mapList");
    }

    private final Term<A> bind = (new Bind<A>()).term();
    private final Term<A> cons = (new Cons<A>()).term();
    private final Term<A> pure = (new Pure<A>()).term();
    private final Term<A> map2 = lambda("x", lambda("y", lambda("fun",
                app(bind, variable("x"), lambda("x1", app(bind, variable("y"),
                        lambda("y1", app(pure, app("f", "a1", "b1")))))))));

    @Override
    public Type<A> type() {
        return Types.lambda("s", "x", "y",
                Types.function(
                        Types.function("x", Types.flow("s", "y")),
                        Types.list("x"),
                        Types.flow("s", Types.list("y"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Term<A> mapping = args.get(0);
            return Flows.map(Expect.list(Flows::pure, args.get(1)), (Function<List<Term<A>>, Term<A>>) terms -> {
                Term<A> appList = list(terms.stream().map(x -> app(mapping, x)).collect(Collectors.toList()));
                Term<A> foldFun = lambda("fList",
                        lambda("fEl", app(map2, variable("fEl"), variable("fList"), cons)));
                return app(Terms.fold(foldFun), app(pure, list()), appList);
            });
        };
    }

    public static <S, X, Y> Function<List<X>, Flow<S, List<Y>>> apply(Function<X, Flow<S, Y>> mapping) {
        return list -> Flows.mapM(list, mapping);
    }

    public static <S, X, Y> Flow<S, List<Y>> apply(Function<X, Flow<S, Y>> mapping, List<X> list) {
        return apply(mapping).apply(list);
    }
}
