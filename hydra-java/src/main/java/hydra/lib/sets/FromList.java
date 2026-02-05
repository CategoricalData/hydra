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

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Creates a set from a list of elements.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.fromList"
     */
    public Name name() {
        return new Name("hydra.lib.sets.fromList");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that creates a set from a list
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(list("x"), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)), arg -> Terms.set(apply(arg)));
    }

    /**
     * Creates a set from a list of elements.
     * @param <X> the type of elements in the list and set
     * @param arg the list of elements
     * @return a set containing all unique elements from the list
     */
    @SuppressWarnings("unchecked")
    public static <X> Set<X> apply(List<X> arg) {
        return orderedSet(arg);
    }

    /**
     * Creates an ordered set from elements. Uses TreeSet when elements are Comparable
     * (matching Haskell's Data.Set behavior), otherwise LinkedHashSet for insertion order.
     */
    @SuppressWarnings("unchecked")
    static <X> Set<X> orderedSet(java.util.Collection<X> elements) {
        if (elements.isEmpty()) {
            return new LinkedHashSet<>();
        }
        // Check if elements are Comparable
        for (X elem : elements) {
            if (elem != null) {
                if (elem instanceof Comparable) {
                    try {
                        TreeSet<X> result = new TreeSet<>((a, b) -> ((Comparable<X>) a).compareTo(b));
                        result.addAll(elements);
                        return result;
                    } catch (ClassCastException e) {
                        return new LinkedHashSet<>(elements);
                    }
                } else {
                    return new LinkedHashSet<>(elements);
                }
            }
        }
        return new LinkedHashSet<>(elements);
    }
}
