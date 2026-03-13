package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
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
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.maybeTerm(cx, t -> Either.right(t), graph, args.get(1)), maybe -> {
                Term fn = args.get(0);
                if (maybe.isNothing()) {
                    return Either.right(new Term.Either(new hydra.util.Either.Right<>(Terms.optional(Maybe.nothing()))));
                }
                Term val = maybe.fromJust();
                Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                    hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(fn, val));
                if (r.isLeft()) return (Either) r;
                Either<InContext<Error_>, hydra.util.Either<Term, Term>> eitherResult =
                    hydra.extract.core.Core.eitherTerm(cx, t -> Either.right(t), t -> Either.right(t), graph,
                        ((Either.Right<InContext<Error_>, Term>) r).value);
                if (eitherResult.isLeft()) return (Either) eitherResult;
                hydra.util.Either<Term, Term> inner =
                    ((Either.Right<InContext<Error_>, hydra.util.Either<Term, Term>>) eitherResult).value;
                if (inner.isLeft()) {
                    return Either.right(new Term.Either(new hydra.util.Either.Left<>(
                        ((hydra.util.Either.Left<Term, Term>) inner).value)));
                }
                return Either.right(new Term.Either(new hydra.util.Either.Right<>(
                    Terms.optional(Maybe.just(((hydra.util.Either.Right<Term, Term>) inner).value)))));
            });
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
