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
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;


/**
 * Maps a flow function over a set.
 */
public class MapSet extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.mapSet");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x", "y",
                Types.function(
                        Types.function("x", Types.flow("s", "y")),
                        Types.set("x"),
                        Types.flow("s", Types.set("y"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.set(Flows::pure, args.get(1)), set -> {
            Flow<Graph, Set<Term>> result = pure(new HashSet<>());
            for (Term item : set) {
                result = bind(result, s -> bind(pure(Terms.apply(args.get(0), item)), v -> {
                    s.add(v);
                    return pure(s);
                }));
            }
            return bind(result, r -> pure(Terms.set(r)));
        });
    }

    /**
     * Applies a flow function to each element.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function
     * @return the flow of results
     */
    public static <S, X, Y> Function<Set<X>, Flow<S, Set<Y>>> apply(Function<X, Flow<S, Y>> f) {
        return set -> apply(f, set);
    }

    /**
     * Applies a flow function to each element.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function
     * @param set the set
     * @return the flow of results
     */
    public static <S, X, Y> Flow<S, Set<Y>> apply(Function<X, Flow<S, Y>> f, Set<X> set) {
        return Flows.mapM(set, f);
    }
}
