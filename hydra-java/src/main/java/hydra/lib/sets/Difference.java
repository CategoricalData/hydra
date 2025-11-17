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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;

/**
 * Computes the set difference.
 */
public class Difference extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.difference"
     */
    public Name name() {
        return new Name("hydra.lib.sets.difference");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that computes set difference
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(set("x"), set("x"), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map2(
                Expect.set(Flows::pure, args.get(0)),
                Expect.set(Flows::pure, args.get(1)),
                (s1, s2) -> Terms.set(apply(s1, s2)));
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
        Set<X> newSet = new HashSet<>(s1);
        newSet.removeAll(s2);
        return newSet;
    }
}
