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
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Drops elements while predicate is true.
 */
public class DropWhile extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.dropWhile");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term pred = args.get(0);
            return bind(Expect.list(Flows::pure, args.get(1)), lst ->
                dropWhileFlow(pred, lst, 0));
        };
    }

    private static Flow<Graph, Term> dropWhileFlow(Term pred, List<Term> lst, int index) {
        if (index >= lst.size()) {
            return pure(Terms.list(List.of()));
        }
        Term element = lst.get(index);
        Term application = Terms.apply(pred, element);
        return bind(hydra.reduction.Reduction.reduceTerm(true, application), reduced ->
            bind(Expect.boolean_(reduced), b -> {
                if (b) {
                    return dropWhileFlow(pred, lst, index + 1);
                } else {
                    return pure(Terms.list(lst.subList(index, lst.size())));
                }
            }));
    }

    /**
     * Drops elements while the predicate holds.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that drops elements while the predicate holds
     */
    public static <X> Function<List<X>, List<X>> apply(Predicate<X> pred) {
        return lst -> apply((Function<X, Boolean>) x -> pred.test(x), lst);
    }

    /**
     * Drops elements while the predicate (as Function) holds.
     * @param <X> the element type
     * @param pred the predicate as a Function (used by generated code)
     * @return a function that drops elements while the predicate holds
     */
    public static <X> Function<List<X>, List<X>> apply(Function<X, Boolean> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Drops elements while the predicate holds.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @param lst the list to drop from
     * @return the remaining list after dropping
     */
    public static <X> List<X> apply(Predicate<X> pred, List<X> lst) {
        return apply((Function<X, Boolean>) x -> pred.test(x), lst);
    }

    /**
     * Drops elements while the predicate (as Function) holds.
     * @param <X> the element type
     * @param pred the predicate as a Function (used by generated code)
     * @param lst the list to drop from
     * @return the remaining list after dropping
     */
    public static <X> List<X> apply(Function<X, Boolean> pred, List<X> lst) {
        List<X> result = new ArrayList<>(lst);
        while (!result.isEmpty() && pred.apply(result.get(0))) {
            result = result.subList(1, result.size());
        }
        return result;
    }
}
