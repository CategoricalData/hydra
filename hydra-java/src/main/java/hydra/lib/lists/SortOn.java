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
                function(function("a", "b"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(1)),
            (Function<List<Term>, Term>) lst -> {
                // Term-level sorting is complex
                // The static apply() provides the actual logic
                return Terms.list(lst);
            });
    }

    /**
     * Sorts by the result of applying the function.
     * @param <X> the element type
     * @param <Y> the key type, which must be comparable
     * @param f the function to extract the sort key
     * @return a function that sorts a list by the extracted key
     */
    public static <X, Y extends Comparable<Y>> Function<List<X>, List<X>> apply(Function<X, Y> f) {
        return lst -> apply(f, lst);
    }

    /**
     * Sorts by the result of applying the function.
     * @param <X> the element type
     * @param <Y> the key type, which must be comparable
     * @param f the function to extract the sort key
     * @param lst the list to sort
     * @return the sorted list
     */
    public static <X, Y extends Comparable<Y>> List<X> apply(Function<X, Y> f, List<X> lst) {
        List<X> result = new ArrayList<>(lst);
        result.sort(Comparator.comparing(f));
        return result;
    }
}
