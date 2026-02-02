package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

/**
 * Map a function that may fail over a Maybe, collecting results or returning the first error.
 */
public class MapMaybe extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.mapMaybe");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), either(var("z"), var("b"))),
                optional(var("a")),
                either(var("z"), optional(var("b")))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            throw new UnsupportedOperationException("MapMaybe primitive not yet implemented for term evaluation");
        };
    }

    /**
     * Map a function over a Maybe, returning Left on failure or Right with the result.
     */
    public static <A, B, Z> hydra.util.Either<Z, Maybe<B>> apply(
            Function<A, hydra.util.Either<Z, B>> fn,
            Maybe<A> maybe) {
        if (maybe.isNothing()) {
            return new hydra.util.Either.Right<>(Maybe.nothing());
        } else {
            // Use orElse(null) to extract the value since Maybe has no direct getter
            A val = maybe.orElse(null);
            hydra.util.Either<Z, B> result = fn.apply(val);
            if (result.isLeft()) {
                return new hydra.util.Either.Left<>(((hydra.util.Either.Left<Z, B>) result).value);
            }
            return new hydra.util.Either.Right<>(Maybe.just(((hydra.util.Either.Right<Z, B>) result).value));
        }
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<Maybe<A>, hydra.util.Either<Z, Maybe<B>>> apply(
            Function<A, hydra.util.Either<Z, B>> fn) {
        return maybe -> apply(fn, maybe);
    }
}
