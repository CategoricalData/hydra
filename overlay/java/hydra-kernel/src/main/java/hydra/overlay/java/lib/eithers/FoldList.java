package hydra.overlay.java.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Monadic left fold over a list using Either.
 * Type: {@code (a -> b -> Either z a) -> a -> [b] -> Either z a}
 */
public class FoldList extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.foldList");

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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(2)), items -> {
                Term fn = args.get(0);
                Term init = args.get(1);
                Term acc = init;
                for (Term item : items) {
                    Term applied = Terms.apply(Terms.apply(fn, acc), item);
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, applied);
                    if (r.isLeft()) return r;
                    Either<Error_, hydra.overlay.java.util.Either<Term, Term>> eitherResult =
                        hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph,
                            ((Either.Right<Error_, Term>) r).value);
                    if (eitherResult.isLeft()) return (Either) eitherResult;
                    hydra.overlay.java.util.Either<Term, Term> result =
                        ((Either.Right<Error_, hydra.overlay.java.util.Either<Term, Term>>) eitherResult).value;
                    if (result.isLeft()) {
                        return Either.right(new Term.Either(new hydra.overlay.java.util.Either.Left<>(
                            ((hydra.overlay.java.util.Either.Left<Term, Term>) result).value)));
                    }
                    acc = ((hydra.overlay.java.util.Either.Right<Term, Term>) result).value;
                }
                return Either.right(new Term.Either(new hydra.overlay.java.util.Either.Right<>(acc)));
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
    public static <A, B, Z> hydra.overlay.java.util.Either<Z, A> apply(
            Function<A, Function<B, hydra.overlay.java.util.Either<Z, A>>> fn,
            A init,
            List<B> items) {
        return apply((a, b) -> fn.apply(a).apply(b), init, items);
    }

    public static <A, B, Z> hydra.overlay.java.util.Either<Z, A> apply(
            BiFunction<A, B, hydra.overlay.java.util.Either<Z, A>> fn,
            A init,
            List<B> items) {
        A acc = init;
        for (B item : items) {
            hydra.overlay.java.util.Either<Z, A> result = fn.apply(acc, item);
            if (result.isLeft()) {
                return result;
            }
            acc = ((hydra.overlay.java.util.Either.Right<Z, A>) result).value;
        }
        return new hydra.overlay.java.util.Either.Right<>(acc);
    }
}
