package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Monadic left fold over a list using Either.
 * Type: (a -> b -> Either z a) -> a -> [b] -> Either z a
 */
public class Foldl extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.foldl");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), function(var("b"), either(var("z"), var("a")))),
                var("a"),
                list(var("b")),
                either(var("z"), var("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(2)), items -> {
                Term fn = args.get(0);
                Term init = args.get(1);
                Term acc = init;
                for (Term item : items) {
                    Term applied = Terms.apply(Terms.apply(fn, acc), item);
                    Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.lexical.Lexical.emptyContext(), graph, true, applied);
                    if (r.isLeft()) return r;
                    Either<InContext<Error_>, hydra.util.Either<Term, Term>> eitherResult =
                        hydra.extract.core.Core.eitherTerm(cx, t -> Either.right(t), t -> Either.right(t), graph,
                            ((Either.Right<InContext<Error_>, Term>) r).value);
                    if (eitherResult.isLeft()) return (Either) eitherResult;
                    hydra.util.Either<Term, Term> result =
                        ((Either.Right<InContext<Error_>, hydra.util.Either<Term, Term>>) eitherResult).value;
                    if (result.isLeft()) {
                        return Either.right(new Term.Either(new hydra.util.Either.Left<>(
                            ((hydra.util.Either.Left<Term, Term>) result).value)));
                    }
                    acc = ((hydra.util.Either.Right<Term, Term>) result).value;
                }
                return Either.right(new Term.Either(new hydra.util.Either.Right<>(acc)));
            });
    }

    /**
     * Monadic left fold: fold a list with a function that may fail, short-circuiting on the first Left.
     *
     * @param <A> the accumulator type
     * @param <B> the element type
     * @param <Z> the error type
     * @param fn the folding function
     * @param init the initial accumulator
     * @param items the list to fold over
     * @return Left on first failure, or Right with the final accumulator
     */
    public static <A, B, Z> hydra.util.Either<Z, A> apply(
            Function<A, Function<B, hydra.util.Either<Z, A>>> fn,
            A init,
            List<B> items) {
        return apply((a, b) -> fn.apply(a).apply(b), init, items);
    }

    public static <A, B, Z> hydra.util.Either<Z, A> apply(
            BiFunction<A, B, hydra.util.Either<Z, A>> fn,
            A init,
            List<B> items) {
        A acc = init;
        for (B item : items) {
            hydra.util.Either<Z, A> result = fn.apply(acc, item);
            if (result.isLeft()) {
                return result;
            }
            acc = ((hydra.util.Either.Right<Z, A>) result).value;
        }
        return new hydra.util.Either.Right<>(acc);
    }
}
