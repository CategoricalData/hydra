package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.pair;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Splits a list at the first element not matching a predicate.
 */
public class Span extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.span().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"),
            pair(list("a"), list("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph ->
            hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(1)), lst -> {
                ConsList<Term> remaining = ConsList.fromList(lst);
                ConsList<Term> takenRev = ConsList.empty();
                while (!remaining.isEmpty()) {
                    Term x = remaining.head();
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, Boolean> b = hydra.core.extract.Model.boolean_(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (!((Either.Right<Error_, Boolean>) b).value) break;
                    takenRev = ConsList.cons(x, takenRev);
                    remaining = remaining.tail();
                }
                return Either.right(Terms.pair(
                    Terms.list(takenRev.reverse()),
                    Terms.list(remaining)));
            });
    }

    /**
     * Splits when predicate becomes false.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that splits a list when the predicate becomes false
     */
    public static <X> Function<List<X>, Pair<List<X>, List<X>>> apply(Function<X, Boolean> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Splits when predicate becomes false.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @param lst the list to split
     * @return a pair of lists, split at the first element where predicate is false
     */
    public static <X> Pair<List<X>, List<X>> apply(Function<X, Boolean> pred, List<X> lst) {
        ConsList<X> remaining = ConsList.fromList(lst);
        ConsList<X> takenRev = ConsList.empty();
        while (!remaining.isEmpty() && pred.apply(remaining.head())) {
            takenRev = ConsList.cons(remaining.head(), takenRev);
            remaining = remaining.tail();
        }
        return new Pair<>(takenRev.reverse(), remaining);
    }
}
