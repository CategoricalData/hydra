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
import java.util.stream.Collectors;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Filters a list, keeping only elements that satisfy the predicate.
 */
public class Filter extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.filter");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term pred = args.get(0);
            return bind(Expect.list(Flows::pure, args.get(1)), lst -> {
                // Apply predicate to each element and collect results
                Flow<Graph, List<Term>> filtered = pure(new ArrayList<>());
                for (Term element : lst) {
                    Term application = Terms.apply(pred, element);
                    filtered = bind(filtered, acc ->
                        bind(hydra.reduction.Reduction.reduceTerm(true, application), reduced ->
                            bind(Expect.boolean_(reduced), b -> {
                                if (b) {
                                    List<Term> newAcc = new ArrayList<>(acc);
                                    newAcc.add(element);
                                    return pure(newAcc);
                                } else {
                                    return pure(acc);
                                }
                            })));
                }
                return Flows.map(filtered, Terms::list);
            });
        };
    }

    /**
     * Filters a list, keeping only elements that satisfy the predicate.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that filters a list by the predicate
     */
    public static <X> Function<List<X>, List<X>> apply(Predicate<X> pred) {
        return lst -> apply((Function<X, Boolean>) x -> pred.test(x), lst);
    }

    /**
     * Filters a list, keeping only elements that satisfy the predicate.
     * @param <X> the element type
     * @param pred the predicate as a Function (used by generated code)
     * @param lst the list to filter
     * @return a new list containing only elements for which the predicate returns true
     */
    public static <X> List<X> apply(Function<X, Boolean> pred, List<X> lst) {
        return lst.stream().filter(x -> pred.apply(x)).collect(Collectors.toList());
    }
}
