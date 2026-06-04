package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;
import hydra.util.PersistentSet;


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
        return Types.constrained2("x", Types.ORD, "y", Types.ORD,
                function(function("x", "y"), set("x"), set("y")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term mapping = args.get(0);
            return hydra.lib.eithers.Map.apply(arg -> {
                PersistentSet<Term> result = PersistentSet.<Term>empty();
                for (Term e : arg) {
                    result = result.insert(Terms.apply(mapping, e));
                }
                return Terms.set(result);
            }, hydra.extract.Core.set(graph, args.get(1)));
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
        PersistentSet<Y> result = PersistentSet.<Y>empty();
        for (X x : arg) {
            result = result.insert(mapping.apply(x));
        }
        return result;
    }
}
