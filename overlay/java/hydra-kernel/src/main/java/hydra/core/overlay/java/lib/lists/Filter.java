package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Filters a list, keeping only elements that satisfy the predicate.
 */
public class Filter extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.filter().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph ->
            hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(1)), lst -> {
                ConsList<Term> reversed = ConsList.empty();
                for (Term x : lst) {
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, Boolean> b = hydra.core.extract.Model.boolean_(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<Error_, Boolean>) b).value) {
                        reversed = ConsList.cons(x, reversed);
                    }
                }
                return Either.right(Terms.list(reversed.reverse()));
            });
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
        return ConsList.fromList(lst).filter(pred::apply);
    }
}
