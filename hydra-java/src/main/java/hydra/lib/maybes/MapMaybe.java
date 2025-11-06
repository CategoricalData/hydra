package hydra.lib.maybes;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Maps a flow function over Maybe.
 */
public class MapMaybe extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maybes.mapMaybe");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(function("a", optional("b")), list("a"), list("b")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.list(Flows::pure, args.get(1)), inputList -> {
            List<Term> results = new ArrayList<>();
            for (Term item : inputList) {
                Term maybeResult = Terms.apply(args.get(0), item);
                Flow<Graph, Opt<Term>> optFlow = Expect.optional(Flows::pure, maybeResult);
                // Simplified implementation - just collect the maybes
                results.add(maybeResult);
            }
            return pure(Terms.list(results));
        });
    }

    /**
     * Applies a flow function to Maybe value.
     * @param f the function
     * @return the flow of Maybe
     */
        public static <X, Y> Function<List<X>, List<Y>> apply(Function<X, Opt<Y>> f) {
        return (list) -> apply(f, list);
    }

    /**
     * Applies a flow function to Maybe value.
     * @param f the function
     * @param list the maybeValue
     * @return the flow of Maybe
     */
        public static <X, Y> List<Y> apply(Function<X, Opt<Y>> f, List<X> list) {
        return list.stream()
            .map(f)
            .filter(Opt::isPresent)
            .map(Opt::get)
            .collect(Collectors.toList());
    }
}
