package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
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
            Term fn = args.get(0);
            return bind(Expect.optional(Flows::pure, args.get(1)), maybe -> {
                if (maybe.isNothing()) {
                    // Nothing -> Right(Nothing)
                    return pure(new Term.Either(new hydra.util.Either.Right<>(
                        new Term.Maybe(Maybe.nothing()))));
                }
                Term val = maybe.fromJust();
                Term application = Terms.apply(fn, val);
                return bind(hydra.reduction.Reduction.reduceTerm(true, application), reduced ->
                    bind(Expect.<Graph, Term, Term>either(reduced), e -> {
                        if (e.isLeft()) {
                            return pure(new Term.Either(new hydra.util.Either.Left<>(
                                ((hydra.util.Either.Left<Term, Term>) e).value)));
                        } else {
                            return pure(new Term.Either(new hydra.util.Either.Right<>(
                                new Term.Maybe(Maybe.just(
                                    ((hydra.util.Either.Right<Term, Term>) e).value)))));
                        }
                    }));
            });
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
