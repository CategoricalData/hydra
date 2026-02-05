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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Converts a set to a list of elements.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.toList"
     */
    public Name name() {
        return new Name("hydra.lib.sets.toList");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a set to a list
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(set("x"), list("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(0)), terms -> {
            java.util.List<Term> sorted = new java.util.ArrayList<>(terms);
            sorted.sort(hydra.lib.equality.Compare::compareTerms);
            return Terms.list(sorted);
        });
    }

    /**
     * Converts a set to a list of elements.
     * @param <X> the type of elements in the set and list
     * @param arg the set to convert
     * @return a list containing all elements from the set
     */
    @SuppressWarnings("unchecked")
    public static <X> List<X> apply(Set<X> arg) {
        // If the set is already ordered (TreeSet), just convert directly.
        // Otherwise, sort if elements are Comparable to match Haskell's Data.Set.toList behavior.
        List<X> result = new ArrayList<>(arg);
        if (!(arg instanceof java.util.SortedSet) && !result.isEmpty() && result.get(0) instanceof Comparable) {
            result.sort((a, b) -> ((Comparable<X>) a).compareTo(b));
        }
        return result;
    }
}
