package hydra.core.overlay.java.lib.eithers;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.set;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentSet;

/**
 * Map a function that may fail over a set, collecting results or returning the first error.
 */
public class MapSet extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.eithers.mapSet");

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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.set(graph, args.get(1)), items -> {
                Term fn = args.get(0);
                PersistentSet<Term> results = PersistentSet.<Term>empty();
                for (Term element : items) {
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(fn, element));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, hydra.core.overlay.java.util.Either<Term, Term>> eitherResult =
                        hydra.core.extract.Model.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph,
                            ((Either.Right<Error_, Term>) r).value);
                    if (eitherResult.isLeft()) return (Either) eitherResult;
                    hydra.core.overlay.java.util.Either<Term, Term> inner =
                        ((Either.Right<Error_, hydra.core.overlay.java.util.Either<Term, Term>>) eitherResult).value;
                    if (inner.isLeft()) {
                        return Either.right(new Term.Either(new hydra.core.overlay.java.util.Either.Left<>(
                            ((hydra.core.overlay.java.util.Either.Left<Term, Term>) inner).value)));
                    }
                    results = results.insert(((hydra.core.overlay.java.util.Either.Right<Term, Term>) inner).value);
                }
                return Either.right(new Term.Either(new hydra.core.overlay.java.util.Either.Right<>(Terms.set(results))));
            });
    }

    /**
     * Map a function over a set, returning Left on the first failure or Right with all results.
     */
    public static <A, B, Z> hydra.core.overlay.java.util.Either<Z, Set<B>> apply(
            Function<A, hydra.core.overlay.java.util.Either<Z, B>> fn,
            Set<A> items) {
        PersistentSet<B> results = PersistentSet.<B>empty();
        for (A item : items) {
            hydra.core.overlay.java.util.Either<Z, B> result = fn.apply(item);
            if (result.isLeft()) {
                return new hydra.core.overlay.java.util.Either.Left<>(((hydra.core.overlay.java.util.Either.Left<Z, B>) result).value);
            }
            results = results.insert(((hydra.core.overlay.java.util.Either.Right<Z, B>) result).value);
        }
        return new hydra.core.overlay.java.util.Either.Right<>(results);
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<Set<A>, hydra.core.overlay.java.util.Either<Z, Set<B>>> apply(
            Function<A, hydra.core.overlay.java.util.Either<Z, B>> fn) {
        return items -> apply(fn, items);
    }
}
