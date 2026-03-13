package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Map a function that may fail over a set, collecting results or returning the first error.
 */
public class MapSet extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.mapSet");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), either(var("z"), var("b"))),
                set(var("a")),
                either(var("z"), set(var("b")))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.set(cx, graph, args.get(1)), items -> {
                Term fn = args.get(0);
                Set<Term> results = new HashSet<>();
                for (Term element : items) {
                    Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(fn, element));
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
                    results.add(((hydra.util.Either.Right<Term, Term>) inner).value);
                }
                return Either.right(new Term.Either(new hydra.util.Either.Right<>(Terms.set(results))));
            });
    }

    /**
     * Map a function over a set, returning Left on the first failure or Right with all results.
     */
    public static <A, B, Z> hydra.util.Either<Z, Set<B>> apply(
            Function<A, hydra.util.Either<Z, B>> fn,
            Set<A> items) {
        Set<B> results = new HashSet<>();
        for (A item : items) {
            hydra.util.Either<Z, B> result = fn.apply(item);
            if (result.isLeft()) {
                return new hydra.util.Either.Left<>(((hydra.util.Either.Left<Z, B>) result).value);
            }
            results.add(((hydra.util.Either.Right<Z, B>) result).value);
        }
        return new hydra.util.Either.Right<>(results);
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<Set<A>, hydra.util.Either<Z, Set<B>>> apply(
            Function<A, hydra.util.Either<Z, B>> fn) {
        return items -> apply(fn, items);
    }
}
