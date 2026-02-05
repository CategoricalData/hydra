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
import hydra.util.Tuple;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;


/**
 * Splits a list at the first element not matching a predicate.
 */
public class Span extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.span");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"),
            pair(list("a"), list("a"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term pred = args.get(0);
            return Flows.bind(Expect.list(Flows::pure, args.get(1)), lst ->
                spanFlow(pred, lst, 0));
        };
    }

    private static Flow<Graph, Term> spanFlow(Term pred, List<Term> lst, int index) {
        if (index >= lst.size()) {
            return pure(Terms.pair(Terms.list(lst), Terms.list(List.of())));
        }
        Term element = lst.get(index);
        Term application = Terms.apply(pred, element);
        return Flows.bind(hydra.reduction.Reduction.reduceTerm(true, application), reduced ->
            Flows.bind(Expect.boolean_(reduced), b -> {
                if (b) {
                    return spanFlow(pred, lst, index + 1);
                } else {
                    return pure(Terms.pair(
                        Terms.list(new ArrayList<>(lst.subList(0, index))),
                        Terms.list(new ArrayList<>(lst.subList(index, lst.size())))));
                }
            }));
    }

    /**
     * Splits when predicate becomes false.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that splits a list when the predicate becomes false
     */
    public static <X> Function<List<X>, Tuple.Tuple2<List<X>, List<X>>> apply(Function<X, Boolean> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Splits when predicate becomes false.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @param lst the list to split
     * @return a pair of lists, split at the first element where predicate is false
     */
    public static <X> Tuple.Tuple2<List<X>, List<X>> apply(Function<X, Boolean> pred, List<X> lst) {
        int i = 0;
        while (i < lst.size() && pred.apply(lst.get(i))) {
            i++;
        }
        return new Tuple.Tuple2<>(new ArrayList<>(lst.subList(0, i)),
            new ArrayList<>(lst.subList(i, lst.size())));
    }
}
