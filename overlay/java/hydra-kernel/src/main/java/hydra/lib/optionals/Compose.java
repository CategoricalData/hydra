package hydra.lib.optionals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Composes two Optional-returning functions.
 */
public class Compose extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.optionals.compose");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c",
                function(function("a", optional("b")),
                        function("b", optional("c")),
                        function("a", optional("c"))));
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Function<Term, Either<Error_, Optional<Term>>> nativeF = val ->
                hydra.lib.eithers.Bind.apply(
                    hydra.Reduction.reduceTerm(hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), val)),
                    reduced -> hydra.extract.Core.optionalTerm(t -> Either.right(t), graph, reduced));
            Function<Term, Either<Error_, Optional<Term>>> nativeG = val ->
                hydra.lib.eithers.Bind.apply(
                    hydra.Reduction.reduceTerm(hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(1), val)),
                    reduced -> hydra.extract.Core.optionalTerm(t -> Either.right(t), graph, reduced));
            return hydra.lib.eithers.Bind.apply(nativeF.apply(args.get(2)), maybeB -> {
                if (!maybeB.isGiven()) {
                    return Either.right(Terms.optional(Optional.none()));
                }
                return hydra.lib.eithers.Map.apply(
                    m -> Terms.optional(m),
                    nativeG.apply(maybeB.fromGiven()));
            });
        };
    }

    /**
     * Composes two Optional-returning functions. Curried version.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @return a function that takes the second function and returns the composed function
     */
    public static <A, B, C> Function<Function<B, Optional<C>>, Function<A, Optional<C>>> apply(Function<A, Optional<B>> left) {
        return right -> apply(left, right);
    }

    /**
     * Composes two Optional-returning functions.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @param right the second function to apply
     * @return a composed function that applies left then right, returning empty if either returns empty
     */
    public static <A, B, C> Function<A, Optional<C>> apply(Function<A, Optional<B>> left, Function<B, Optional<C>> right) {
        return a -> {
            Optional<B> ob = left.apply(a);
            return ob.isGiven() ? right.apply(ob.fromGiven()) : Optional.none();
        };
    }

    /**
     * Composes two Optional-returning functions and applies to a value.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @param right the second function to apply
     * @param a the value to apply the composed function to
     * @return the result of applying the composed function to the value
     */
    public static <A, B, C> Optional<C> apply(Function<A, Optional<B>> left, Function<B, Optional<C>> right, A a) {
        return apply(left, right).apply(a);
    }
}
