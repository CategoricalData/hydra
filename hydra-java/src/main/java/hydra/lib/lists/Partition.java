package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(1)), lst -> {
                List<Term> yes = new ArrayList<>();
                List<Term> no = new ArrayList<>();
                for (Term x : lst) {
                    Either<InContext<OtherError>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        new hydra.context.Context(java.util.List.of(), java.util.List.of(), java.util.Map.of()), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<InContext<OtherError>, Boolean> b = hydra.extract.core.Core.boolean_(cx, graph,
                        ((Either.Right<InContext<OtherError>, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<InContext<OtherError>, Boolean>) b).value) {
                        yes.add(x);
                    } else {
                        no.add(x);
                    }
                }
                return Either.right(Terms.pair(Terms.list(yes), Terms.list(no)));
            });
    }

    /**
     * Partitions a list based on a predicate.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that partitions a list by the predicate
     */
    public static <X> Function<List<X>, Pair<List<X>, List<X>>> apply(Function<X, Boolean> pred) {
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
    public static <X> Pair<List<X>, List<X>> apply(Function<X, Boolean> pred, List<X> lst) {
        List<X> yes = new ArrayList<>();
        List<X> no = new ArrayList<>();
        for (X x : lst) {
            if (pred.apply(x)) {
                yes.add(x);
            } else {
                no.add(x);
            }
        }
        return new Pair<>(yes, no);
    }
}
