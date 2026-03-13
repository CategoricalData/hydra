package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(1)), lst -> {
                List<Term> results = new ArrayList<>();
                for (Term x : lst) {
                    Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<InContext<Error_>, Boolean> b = hydra.extract.core.Core.boolean_(cx, graph,
                        ((Either.Right<InContext<Error_>, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<InContext<Error_>, Boolean>) b).value) {
                        results.add(x);
                    }
                }
                return Either.right(Terms.list(results));
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
        return lst.stream().filter(x -> pred.apply(x)).collect(Collectors.toList());
    }
}
