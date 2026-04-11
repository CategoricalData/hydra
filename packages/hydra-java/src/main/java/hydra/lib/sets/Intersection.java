package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.TreeSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeOrd;
import static hydra.dsl.Types.set;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Computes the intersection of two sets.
 */
public class Intersection extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.intersection"
     */
    public Name name() {
        return new Name("hydra.lib.sets.intersection");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that computes set intersection
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(set("x"), set("x"), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.set(graph, args.get(0)), s1 -> hydra.lib.eithers.Map.apply(s2 -> Terms.set(apply(s1, s2)), hydra.extract.Core.set(graph, args.get(1))));
    }

    /**
     * Computes the intersection of two sets.
     * @param <X> the type of elements in the sets
     * @param s1 the first set
     * @return a function that takes the second set and returns the intersection
     */
    public static <X> Function<Set<X>, Set<X>> apply(Set<X> s1) {
        return (s2) -> apply(s1, s2);
    }

    /**
     * Computes the intersection of two sets.
     * @param <X> the type of elements in the sets
     * @param s1 the first set
     * @param s2 the second set
     * @return a new set containing elements common to both sets
     */
    public static <X> Set<X> apply(Set<X> s1, Set<X> s2) {
        Set<X> result = new TreeSet<>(s1);
        result.retainAll(s2);
        return result;
    }
}
