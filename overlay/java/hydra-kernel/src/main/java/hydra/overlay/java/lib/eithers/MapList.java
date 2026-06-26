package hydra.overlay.java.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;

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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(1)), lst -> {
                Term fn = args.get(0);
                ConsList<Term> reversed = ConsList.empty();
                for (Term element : lst) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(fn, element));
                    if (r.isLeft()) return r;
                    Either<Error_, hydra.overlay.java.util.Either<Term, Term>> eitherResult =
                        hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph,
                            ((Either.Right<Error_, Term>) r).value);
                    if (eitherResult.isLeft()) return (Either) eitherResult;
                    hydra.overlay.java.util.Either<Term, Term> inner =
                        ((Either.Right<Error_, hydra.overlay.java.util.Either<Term, Term>>) eitherResult).value;
                    if (inner.isLeft()) {
                        return Either.right(new Term.Either(new hydra.overlay.java.util.Either.Left<>(
                            ((hydra.overlay.java.util.Either.Left<Term, Term>) inner).value)));
                    }
                    reversed = ConsList.cons(((hydra.overlay.java.util.Either.Right<Term, Term>) inner).value, reversed);
                }
                return Either.right(new Term.Either(new hydra.overlay.java.util.Either.Right<>(Terms.list(reversed.reverse()))));
            });
    }

    /**
     * Map a function over a list, returning Left on the first failure or Right with all results.
     */
    public static <A, B, Z> hydra.overlay.java.util.Either<Z, List<B>> apply(
            Function<A, hydra.overlay.java.util.Either<Z, B>> fn,
            List<A> items) {
        ConsList<B> reversed = ConsList.empty();
        for (A item : items) {
            hydra.overlay.java.util.Either<Z, B> result = fn.apply(item);
            if (result.isLeft()) {
                return new hydra.overlay.java.util.Either.Left<>(((hydra.overlay.java.util.Either.Left<Z, B>) result).value);
            }
            reversed = ConsList.cons(((hydra.overlay.java.util.Either.Right<Z, B>) result).value, reversed);
        }
        return new hydra.overlay.java.util.Either.Right<>(reversed.reverse());
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<List<A>, hydra.overlay.java.util.Either<Z, List<B>>> apply(
            Function<A, hydra.overlay.java.util.Either<Z, B>> fn) {
        return items -> apply(fn, items);
    }
}
