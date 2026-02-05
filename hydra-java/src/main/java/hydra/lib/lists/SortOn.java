package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;


/**
 * Sorts a list by a key function.
 */
public class SortOn extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.sortOn");
    }

    @Override
    public TypeScheme type() {
        return new hydra.core.TypeScheme(
                Arrays.asList(new hydra.core.Name("a"), new hydra.core.Name("b")),
                function(function("a", "b"), list("a"), list("a")),
                Maybe.nothing());
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term f = args.get(0);
            return Flows.bind(Expect.list(Flows::pure, args.get(1)), lst -> {
                // Compute sort keys for each element
                Flow<Graph, List<hydra.util.Tuple.Tuple2<Term, Term>>> pairsFlow = pure(new ArrayList<>());
                for (Term element : lst) {
                    Term application = Terms.apply(f, element);
                    pairsFlow = Flows.bind(pairsFlow, acc ->
                        Flows.map(hydra.reduction.Reduction.reduceTerm(true, application), key -> {
                            acc.add(new hydra.util.Tuple.Tuple2<>(key, element));
                            return acc;
                        }));
                }
                return Flows.map(pairsFlow, pairs -> {
                    pairs.sort((a, b) -> hydra.lib.equality.Compare.compareTerms(a.object1, b.object1));
                    List<Term> sorted = new ArrayList<>();
                    for (hydra.util.Tuple.Tuple2<Term, Term> p : pairs) {
                        sorted.add(p.object2);
                    }
                    return Terms.list(sorted);
                });
            });
        };
    }

    /**
     * Sorts by the result of applying the function.
     * @param <X> the element type
     * @param <Y> the key type (expected to be Comparable at runtime)
     * @param f the function to extract the sort key
     * @return a function that sorts a list by the extracted key
     */
    @SuppressWarnings("unchecked")
    public static <X, Y> Function<List<X>, List<X>> apply(Function<X, Y> f) {
        return lst -> apply(f, lst);
    }

    /**
     * Sorts by the result of applying the function.
     * @param <X> the element type
     * @param <Y> the key type (expected to be Comparable at runtime)
     * @param f the function to extract the sort key
     * @param lst the list to sort
     * @return the sorted list
     */
    @SuppressWarnings("unchecked")
    public static <X, Y> List<X> apply(Function<X, Y> f, List<X> lst) {
        List<X> result = new ArrayList<>(lst);
        result.sort(Comparator.comparing(x -> (Comparable) f.apply(x)));
        return result;
    }
}
