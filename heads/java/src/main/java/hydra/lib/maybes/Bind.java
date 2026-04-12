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
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Monadic bind for optional values (flatMap).
 * If the optional is Just, applies the function; if Nothing, returns Nothing.
 */
public class Bind extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.bind"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.bind");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for monadic bind on optional values
     */
    @Override
    public TypeScheme type() {
        return scheme("a","b",
                function(optional("a"), function("a", optional("b")), optional("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that performs monadic bind on optional values
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.maybeTerm(t -> Either.right(t), graph, args.get(0)), arg -> {
                if (arg.isNothing()) {
                    return Either.right(Terms.optional(Maybe.nothing()));
                }
                Term val = arg.fromJust();
                Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                    hydra.Lexical.emptyContext(), graph, true, Terms.apply(args.get(1), val));
                if (r.isLeft()) return (Either) r;
                Either<Error_, Maybe<Term>> maybeResult = hydra.extract.Core.maybeTerm(
                    t -> Either.right(t), graph, ((Either.Right<Error_, Term>) r).value);
                if (maybeResult.isLeft()) return (Either) maybeResult;
                return Either.right(Terms.optional(((Either.Right<Error_, Maybe<Term>>) maybeResult).value));
            });
    }

    /**
     * Chains optional computations (curried version).
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalArg the optional value to bind
     * @return a function that takes a binding function and returns an optional result
     */
    public static <X, Y> Function<Function<X, Maybe<Y>>, Maybe<Y>> apply(Maybe<X> optionalArg) {
        return (f) -> apply(optionalArg, f);
    }

    /**
     * Applies monadic bind to an optional value with a binding function.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalArg the optional value to bind
     * @param f the binding function
     * @return the optional result of applying the binding function, or empty if the input is empty
     */
    public static <X, Y> Maybe<Y> apply(Maybe<X> optionalArg, Function<X, Maybe<Y>> f) {
        return optionalArg.isJust()
            ? f.apply(optionalArg.fromJust())
            : Maybe.nothing();
    }
}
