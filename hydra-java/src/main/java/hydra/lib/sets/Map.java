package hydra.lib.sets;

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

import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Maps a function over all elements in a set.
 */
public class Map extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.map"
     */
    public Name name() {
        return new Name("hydra.lib.sets.map");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that maps over set elements
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
                function(function("x", "y"), set("x"), set("y")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term mapping = args.get(0);
            return Flows.map(Expect.set(Flows::pure, args.get(1)),
                arg -> Terms.set(FromList.orderedSet(arg.stream().map(e -> Terms.apply(mapping, e)).collect(Collectors.toList()))));
        };
    }

    /**
     * Maps a function over all elements in a set.
     * @param <X> the type of elements in the input set
     * @param <Y> the type of elements in the output set
     * @param mapping the function to apply to each element
     * @return a function that takes a set and returns a new set with the function applied
     */
    public static <X, Y> Function<Set<X>, Set<Y>> apply(Function<X, Y> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Maps a function over all elements in a set.
     * @param <X> the type of elements in the input set
     * @param <Y> the type of elements in the output set
     * @param mapping the function to apply to each element
     * @param arg the set to transform
     * @return a new set with the function applied to all elements
     */
    public static <X, Y> Set<Y> apply(Function<X, Y> mapping, Set<X> arg) {
        java.util.List<Y> mapped = arg.stream().map(mapping).collect(Collectors.toList());
        return FromList.orderedSet(mapped);
    }
}
