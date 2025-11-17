package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
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
import static hydra.dsl.Terms.variable;


/**
 * Maps a flow function over a list.
 */
public class MapList extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.mapList");
    }

    private final Term bind = (new Bind()).term();
    private final Term cons = (new Cons()).term();
    private final Term pure = (new Pure()).term();
    private final Term map2 = lambda("x", lambda("y", lambda("fun",
                app(bind, variable("x"), lambda("x1", app(bind, variable("y"),
                        lambda("y1", app(pure, app(variable("fun"), variable("x1"), variable("y1"))))))))));

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x", "y",
                Types.function(
                        Types.function("x", Types.flow("s", "y")),
                        Types.list("x"),
                        Types.flow("s", Types.list("y"))));
    }

    // TODO: restore when Terms.fold() / Elimination.List is generated
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            throw new UnsupportedOperationException(
                    "MapList.implementation() not yet available - requires Elimination.List");
        };
    }
    // @Override
    // protected Function<List<Term>, Flow<Graph, Term>> implementation() {
    //     return args -> {
    //         Term mapping = args.get(0);
    //         return Flows.map(Expect.list(Flows::pure, args.get(1)), (Function<List<Term>, Term>) terms -> {
    //             Term appList = list(terms.stream().map(x -> app(mapping, x)).collect(Collectors.toList()));
    //             Term foldFun = lambda("fList",
    //                     lambda("fEl", app(map2, variable("fEl"), variable("fList"), cons)));
    //             return app(Terms.fold(foldFun), app(pure, list()), appList);
    //         });
    //     };
    // }

    /**
     * Applies a flow function to each element.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param mapping the function
     * @return the flow of results
     */
    public static <S, X, Y> Function<List<X>, Flow<S, List<Y>>> apply(Function<X, Flow<S, Y>> mapping) {
        return list -> Flows.mapM(list, mapping);
    }

    /**
     * Applies a flow function to each element.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param mapping the function
     * @param list the list
     * @return the flow of results
     */
    public static <S, X, Y> Flow<S, List<Y>> apply(Function<X, Flow<S, Y>> mapping, List<X> list) {
        return apply(mapping).apply(list);
    }
}
