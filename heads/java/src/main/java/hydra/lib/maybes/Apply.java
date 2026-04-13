package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Applies a function in a flow context.
 */
public class Apply extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.apply"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.apply");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for applying an optional function to an optional value
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(optional(function("a", "b")), optional("a"), optional("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that applies an optional function to an optional argument
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.Core.maybeTerm(t -> Either.right(t), graph, args.get(0)), optionalF ->
                hydra.lib.eithers.Map.apply(optionalArg ->
                    (optionalF.isJust() && optionalArg.isJust())
                        ? Terms.optional(Maybe.just(Terms.apply(optionalF.fromJust(), optionalArg.fromJust())))
                        : Terms.optional(Maybe.nothing()),
                    hydra.extract.Core.maybeTerm(t -> Either.right(t), graph, args.get(1))));
    }

    /**
     * Applies a function within a flow context. Curried version.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalF the optional function to apply
     * @return a function that takes an optional argument and returns an optional result
     */
    public static <X, Y> Function<Maybe<X>, Maybe<Y>> apply(Maybe<Function<X, Y>> optionalF) {
        return (optionalArg) -> apply(optionalF, optionalArg);
    }

    /**
     * Applies an optional function to an optional argument.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalF the optional function to apply
     * @param optionalArg the optional argument
     * @return the optional result of applying the function to the argument, or empty if either is empty
     */
    public static <X, Y> Maybe<Y> apply(Maybe<Function<X, Y>> optionalF, Maybe<X> optionalArg) {
        if (!optionalF.isJust() || !optionalArg.isJust()) {
            return Maybe.nothing();
        }

        Function<X, Y> f = optionalF.fromJust();
        X arg = optionalArg.fromJust();

        return Maybe.just(f.apply(arg));
    }
}
