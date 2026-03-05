package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
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
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(arg -> Terms.set(apply(arg)), hydra.extract.core.Core.list(cx, graph, args.get(0)));
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
                        // Guard against compareTo/equals inconsistency: if TreeSet lost
                        // elements (compareTo returns 0 for non-equal objects), fall back
                        // to LinkedHashSet which uses equals/hashCode correctly.
                        LinkedHashSet<X> lhs = new LinkedHashSet<>(elements);
                        if (result.size() < lhs.size()) {
                            return lhs;
                        }
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
