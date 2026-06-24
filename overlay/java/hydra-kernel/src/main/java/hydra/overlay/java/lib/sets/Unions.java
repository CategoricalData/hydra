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
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import static hydra.overlay.java.dsl.Types.set;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.PersistentSet;


/**
 * Computes the union of multiple sets.
 */
public class Unions extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.unions"
     */
    public Name name() {
        return hydra.lib.Sets.unions().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that computes the union of multiple sets
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(list(set("x")), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(sets -> Terms.set(apply(sets)), hydra.extract.Core.listOf(t -> hydra.extract.Core.set(graph, t), graph, args.get(0)));
    }

    /**
     * Computes the union of multiple sets.
     * @param <X> the type of elements in the sets
     * @param sets the list of sets to combine
     * @return a new set containing all elements from all input sets
     */
    public static <X> Set<X> apply(List<Set<X>> sets) {
        PersistentSet<X> result = PersistentSet.<X>empty();
        for (Set<X> s : sets) {
            result = result.union(PersistentSet.<X>coerce(s));
        }
        return result;
    }
}
