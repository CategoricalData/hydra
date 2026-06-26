package hydra.overlay.java.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import static hydra.overlay.java.dsl.Types.set;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.PersistentSet;

/**
 * Computes the set difference.
 */
public class Difference extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.difference"
     */
    public Name name() {
        return hydra.lib.Sets.difference().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that computes set difference
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.set(graph, args.get(0)), s1 -> hydra.overlay.java.lib.eithers.Map.apply(s2 -> Terms.set(apply(s1, s2)), hydra.extract.Core.set(graph, args.get(1))));
    }

    /**
     * Computes the set difference by removing elements of the second set from the first.
     * @param <X> the type of elements in the sets
     * @param s1 the first set
     * @return a function that takes the second set and returns the set difference
     */
    public static <X> Function<Set<X>, Set<X>> apply(Set<X> s1) {
        return (s2) -> apply(s1, s2);
    }

    /**
     * Computes the set difference by removing elements of the second set from the first.
     * @param <X> the type of elements in the sets
     * @param s1 the first set
     * @param s2 the second set
     * @return a new set containing elements in s1 but not in s2
     */
    public static <X> Set<X> apply(Set<X> s1, Set<X> s2) {
        return PersistentSet.<X>coerce(s1).difference(PersistentSet.<X>coerce(s2));
    }
}
