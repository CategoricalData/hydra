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
 * Maps a function over a flow.
 */
public class Map extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.map"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.map");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for mapping a function over an optional value
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(function("a", "b"), optional("a"), optional("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that maps a function over an optional value
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.maybeTerm(cx, instance -> Either.right(Terms.apply(args.get(0), instance)), graph, args.get(1)), opt -> Either.right(Terms.optional(opt)));
    }

    /**
     * Maps a function over an optional value. Curried version.
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function to map
     * @return a function that takes an optional and returns a mapped optional
     */
    public static <X, Y> Function<Maybe<X>, Maybe<Y>> apply(Function<X, Y> f) {
        return (optionalArg) -> apply(f, optionalArg);
    }

    /**
     * Maps a function over an optional value.
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function to map
     * @param optionalArg the optional value to map over
     * @return an optional containing the mapped value if present, otherwise empty
     */
    public static <X, Y> Maybe<Y> apply(Function<X, Y> f, Maybe<X> optionalArg) {
        if (!optionalArg.isJust()) {
            return Maybe.nothing();
        }

        X arg = optionalArg.fromJust();

        return Maybe.just(f.apply(arg));
    }
}
