package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
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
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(1)), lst -> {
                ConsList<Term> yesRev = ConsList.empty();
                ConsList<Term> noRev = ConsList.empty();
                for (Term x : lst) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, Boolean> b = hydra.extract.Core.boolean_(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<Error_, Boolean>) b).value) {
                        yesRev = ConsList.cons(x, yesRev);
                    } else {
                        noRev = ConsList.cons(x, noRev);
                    }
                }
                return Either.right(Terms.pair(Terms.list(yesRev.reverse()), Terms.list(noRev.reverse())));
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
        ConsList<X> yesRev = ConsList.empty();
        ConsList<X> noRev = ConsList.empty();
        for (X x : lst) {
            if (pred.apply(x)) {
                yesRev = ConsList.cons(x, yesRev);
            } else {
                noRev = ConsList.cons(x, noRev);
            }
        }
        return new Pair<>(yesRev.reverse(), noRev.reverse());
    }
}
