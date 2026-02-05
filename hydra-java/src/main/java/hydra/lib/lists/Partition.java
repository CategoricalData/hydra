package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Tuple;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;


/**
 * Partitions a list based on a predicate.
 * Returns (elements satisfying predicate, elements not satisfying predicate).
 */
public class Partition extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.partition");
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
            return bind(Expect.list(Flows::pure, args.get(1)), lst -> {
                Flow<Graph, Tuple.Tuple2<List<Term>, List<Term>>> partitioned =
                    pure(new Tuple.Tuple2<>(new ArrayList<>(), new ArrayList<>()));
                for (Term element : lst) {
                    Term application = Terms.apply(pred, element);
                    partitioned = bind(partitioned, acc ->
                        bind(hydra.reduction.Reduction.reduceTerm(true, application), reduced ->
                            bind(Expect.boolean_(reduced), b -> {
                                List<Term> yes = new ArrayList<>(acc.object1);
                                List<Term> no = new ArrayList<>(acc.object2);
                                if (b) {
                                    yes.add(element);
                                } else {
                                    no.add(element);
                                }
                                return pure(new Tuple.Tuple2<>(yes, no));
                            })));
                }
                return Flows.map(partitioned, p ->
                    Terms.pair(Terms.list(p.object1), Terms.list(p.object2)));
            });
        };
    }

    /**
     * Partitions a list based on a predicate.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that partitions a list by the predicate
     */
    public static <X> Function<List<X>, Tuple.Tuple2<List<X>, List<X>>> apply(Function<X, Boolean> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Partitions a list based on a predicate.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @param lst the list to partition
     * @return a pair where first contains elements satisfying the predicate,
     *         second contains elements not satisfying the predicate
     */
    public static <X> Tuple.Tuple2<List<X>, List<X>> apply(Function<X, Boolean> pred, List<X> lst) {
        List<X> yes = new ArrayList<>();
        List<X> no = new ArrayList<>();
        for (X x : lst) {
            if (pred.apply(x)) {
                yes.add(x);
            } else {
                no.add(x);
            }
        }
        return new Tuple.Tuple2<>(yes, no);
    }
}
