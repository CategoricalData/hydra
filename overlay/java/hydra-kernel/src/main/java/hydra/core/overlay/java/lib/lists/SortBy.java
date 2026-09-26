package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Sorts a list by a key function.
 */
public class SortBy extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.sortBy().name;
    }

    @Override
    public TypeScheme type() {
        return Types.constrained2("a", Types.NONE, "b", Types.ORD,
                function(function("a", "b"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph ->
            hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(1)), lst -> {
                // Pre-compute all keys so we can short-circuit on error.
                // ArrayList scratch is used here because sorting requires random access.
                ArrayList<Term> indexed = new ArrayList<>(lst);
                ArrayList<Term> keys = new ArrayList<>(indexed.size());
                for (Term x : indexed) {
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    keys.add(((Either.Right<Error_, Term>) r).value);
                }
                Integer[] indices = new Integer[indexed.size()];
                for (int i = 0; i < indices.length; i++) indices[i] = i;
                java.util.Arrays.sort(indices, Comparator.comparing(i -> (Comparable) keys.get(i)));
                ConsList<Term> reversed = ConsList.empty();
                for (int i : indices) reversed = ConsList.cons(indexed.get(i), reversed);
                return Either.right(Terms.list(reversed.reverse()));
            });
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
        ArrayList<X> scratch = new ArrayList<>(lst);
        scratch.sort(Comparator.comparing(x -> (Comparable) f.apply(x)));
        return ConsList.fromList(scratch);
    }
}
