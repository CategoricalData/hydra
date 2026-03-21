package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Composes two Maybe-returning functions.
 */
public class Compose extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maybes.compose");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c",
                function(function("a", optional("b")),
                        function("b", optional("c")),
                        function("a", optional("c"))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Function<Term, Either<InContext<Error_>, Maybe<Term>>> nativeF = val ->
                hydra.lib.eithers.Bind.apply(
                    hydra.reduction.Reduction.reduceTerm(hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(args.get(0), val)),
                    reduced -> hydra.extract.core.Core.maybeTerm(cx, t -> Either.right(t), graph, reduced));
            Function<Term, Either<InContext<Error_>, Maybe<Term>>> nativeG = val ->
                hydra.lib.eithers.Bind.apply(
                    hydra.reduction.Reduction.reduceTerm(hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(args.get(1), val)),
                    reduced -> hydra.extract.core.Core.maybeTerm(cx, t -> Either.right(t), graph, reduced));
            return hydra.lib.eithers.Bind.apply(nativeF.apply(args.get(2)), maybeB -> {
                if (!maybeB.isJust()) {
                    return Either.right(Terms.optional(Maybe.nothing()));
                }
                return hydra.lib.eithers.Map.apply(
                    m -> Terms.optional(m),
                    nativeG.apply(maybeB.fromJust()));
            });
        };
    }

    /**
     * Composes two Maybe-returning functions. Curried version.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @return a function that takes the second function and returns the composed function
     */
    public static <A, B, C> Function<Function<B, Maybe<C>>, Function<A, Maybe<C>>> apply(Function<A, Maybe<B>> left) {
        return right -> apply(left, right);
    }

    /**
     * Composes two Maybe-returning functions.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @param right the second function to apply
     * @return a composed function that applies left then right, returning empty if either returns empty
     */
    public static <A, B, C> Function<A, Maybe<C>> apply(Function<A, Maybe<B>> left, Function<B, Maybe<C>> right) {
        return a -> {
            Maybe<B> ob = left.apply(a);
            return ob.isJust() ? right.apply(ob.fromJust()) : Maybe.nothing();
        };
    }

    /**
     * Composes two Maybe-returning functions and applies to a value.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @param right the second function to apply
     * @param a the value to apply the composed function to
     * @return the result of applying the composed function to the value
     */
    public static <A, B, C> Maybe<C> apply(Function<A, Maybe<B>> left, Function<B, Maybe<C>> right, A a) {
        return apply(left, right).apply(a);
    }
}
