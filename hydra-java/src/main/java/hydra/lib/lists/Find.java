package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Finds the first element in a list that satisfies the predicate.
 */
public class Find extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.find");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(cx, graph, args.get(1)), lst -> {
                for (Term x : lst) {
                    Either<InContext<Error_>, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<InContext<Error_>, Boolean> b = hydra.extract.Core.boolean_(cx, graph,
                        ((Either.Right<InContext<Error_>, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<InContext<Error_>, Boolean>) b).value) {
                        return Either.right(Terms.optional(Maybe.just(x)));
                    }
                }
                return Either.right(Terms.optional(Maybe.nothing()));
            });
    }

    /**
     * Finds the first element in a list that satisfies the predicate.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that finds the first matching element
     */
    public static <X> Function<List<X>, Maybe<X>> apply(Predicate<X> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Finds the first element in a list that satisfies the predicate.
     * @param <X> the element type
     * @param pred the predicate as a Function (used by generated code)
     * @param lst the list to search
     * @return an optional containing the first matching element, or empty if none found
     */
    public static <X> Maybe<X> apply(Function<X, Boolean> pred, List<X> lst) {
        return apply((Predicate<X>) x -> pred.apply(x), lst);
    }

    /**
     * Finds the first element in a list that satisfies the predicate.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @param lst the list to search
     * @return an optional containing the first matching element, or empty if none found
     */
    public static <X> Maybe<X> apply(Predicate<X> pred, List<X> lst) {
        for (X x : lst) {
            if (pred.test(x)) {
                return Maybe.just(x);
            }
        }
        return Maybe.nothing();
    }
}
