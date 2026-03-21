package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Map a function that may fail over a list, collecting results or returning the first error.
 */
public class MapList extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.mapList");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), either(var("z"), var("b"))),
                list(var("a")),
                either(var("z"), list(var("b")))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(1)), lst -> {
                Term fn = args.get(0);
                List<Term> results = new ArrayList<>();
                for (Term element : lst) {
                    Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(fn, element));
                    if (r.isLeft()) return r;
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
                return Either.right(new Term.Either(new hydra.util.Either.Right<>(Terms.list(results))));
            });
    }

    /**
     * Map a function over a list, returning Left on the first failure or Right with all results.
     */
    public static <A, B, Z> hydra.util.Either<Z, ConsList<B>> apply(
            Function<A, hydra.util.Either<Z, B>> fn,
            ConsList<A> items) {
        ArrayList<B> results = new ArrayList<>();
        for (A item : items) {
            hydra.util.Either<Z, B> result = fn.apply(item);
            if (result.isLeft()) {
                return new hydra.util.Either.Left<>(((hydra.util.Either.Left<Z, B>) result).value);
            }
            results.add(((hydra.util.Either.Right<Z, B>) result).value);
        }
        return new hydra.util.Either.Right<>(ConsList.fromList(results));
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<ConsList<A>, hydra.util.Either<Z, ConsList<B>>> apply(
            Function<A, hydra.util.Either<Z, B>> fn) {
        return items -> apply(fn, items);
    }
}
