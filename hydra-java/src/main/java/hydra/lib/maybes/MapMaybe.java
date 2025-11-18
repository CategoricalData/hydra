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
import hydra.util.Maybe;

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
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.mapMaybe"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.mapMaybe");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for mapping an optional-returning function over a list
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(function("a", optional("b")), list("a"), list("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that maps an optional-returning function over a list
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.list(Flows::pure, args.get(1)), inputList -> {
            List<Term> results = new ArrayList<>();
            for (Term item : inputList) {
                Term maybeResult = Terms.apply(args.get(0), item);
                Flow<Graph, Maybe<Term>> optFlow = Expect.optional(Flows::pure, maybeResult);
                // Simplified implementation - just collect the maybes
                results.add(maybeResult);
            }
            return pure(Terms.list(results));
        });
    }

    /**
     * Maps an optional-returning function over a list and collects present values. Curried version.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param f the optional-returning function to map
     * @return a function that takes a list and returns a list of present values
     */
    public static <X, Y> Function<List<X>, List<Y>> apply(Function<X, Maybe<Y>> f) {
        return (list) -> apply(f, list);
    }

    /**
     * Maps an optional-returning function over a list and collects present values.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param f the optional-returning function to map
     * @param list the list to map over
     * @return a list containing only the present values from applying the function
     */
    public static <X, Y> List<Y> apply(Function<X, Maybe<Y>> f, List<X> list) {
        return list.stream()
            .map(f)
            .filter(Maybe::isJust)
            .map(Maybe::fromJust)
            .collect(Collectors.toList());
    }
}
