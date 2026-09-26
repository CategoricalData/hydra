package hydra.core.overlay.java.lib.sets;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import static hydra.core.overlay.java.dsl.Types.set;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentSet;

/**
 * Computes the union of two sets.
 */
public class Union extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.sets.union"
     */
    public Name name() {
        return hydra.core.lib.Sets.union().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that computes set union
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.set(graph, args.get(0)), s1 -> hydra.core.overlay.java.lib.eithers.Map.apply(s2 -> Terms.set(apply(s1, s2)), hydra.core.extract.Model.set(graph, args.get(1))));
    }

    /**
     * Computes the union of two sets.
     * @param <X> the type of elements in the sets
     * @param s1 the first set
     * @return a function that takes the second set and returns the union
     */
    public static <X> Function<Set<X>, Set<X>> apply(Set<X> s1) {
        return (s2) -> apply(s1, s2);
    }

    /**
     * Computes the union of two sets.
     * @param <X> the type of elements in the sets
     * @param s1 the first set
     * @param s2 the second set
     * @return a new set containing all elements from both sets
     */
    public static <X> Set<X> apply(Set<X> s1, Set<X> s2) {
        return PersistentSet.<X>coerce(s1).union(PersistentSet.<X>coerce(s2));
    }
}
