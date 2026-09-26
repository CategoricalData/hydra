package hydra.core.overlay.java.lib.eithers;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Map a function over the Right side of an Either value.
 */
public class Map extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.eithers.map");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), var("b")),
                either(var("z"), var("a")),
                either(var("z"), var("b"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term fn = args.get(0);
            return hydra.core.overlay.java.lib.eithers.Map.apply(e -> e.accept(new hydra.core.overlay.java.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.core.overlay.java.util.Either.Left<Term, Term> left) {
                        return Terms.left(left.value);
                    }

                    @Override
                    public Term visit(hydra.core.overlay.java.util.Either.Right<Term, Term> right) {
                        return Terms.right(Terms.apply(fn, right.value));
                    }
                }), hydra.core.extract.Model.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
        };
    }

    /**
     * Apply a function to the Right side of an Either value.
     */
    public static <A, B, Z> hydra.core.overlay.java.util.Either<Z, B> apply(
            Function<A, B> fn,
            hydra.core.overlay.java.util.Either<Z, A> either) {
        if (either.isLeft()) {
            return new hydra.core.overlay.java.util.Either.Left<>(((hydra.core.overlay.java.util.Either.Left<Z, A>) either).value);
        } else {
            return new hydra.core.overlay.java.util.Either.Right<>(fn.apply(((hydra.core.overlay.java.util.Either.Right<Z, A>) either).value));
        }
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<hydra.core.overlay.java.util.Either<Z, A>, hydra.core.overlay.java.util.Either<Z, B>> apply(
            Function<A, B> fn) {
        return either -> apply(fn, either);
    }
}
