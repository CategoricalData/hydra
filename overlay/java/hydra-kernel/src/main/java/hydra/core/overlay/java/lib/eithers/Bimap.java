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
 * Map over both sides of an Either value.
 */
public class Bimap extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.eithers.bimap");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c", "d",
            function(
                function(var("a"), var("c")),
                function(var("b"), var("d")),
                either(var("a"), var("b")),
                either(var("c"), var("d"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term forLeft = args.get(0);
            Term forRight = args.get(1);
            return hydra.core.overlay.java.lib.eithers.Map.apply(e -> e.accept(new hydra.core.overlay.java.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.core.overlay.java.util.Either.Left<Term, Term> left) {
                        return Terms.left(Terms.apply(forLeft, left.value));
                    }

                    @Override
                    public Term visit(hydra.core.overlay.java.util.Either.Right<Term, Term> right) {
                        return Terms.right(Terms.apply(forRight, right.value));
                    }
                }), hydra.core.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(2)));
        };
    }

    /**
     * Map over both sides of an Either value.
     *
     * @param <A> the original left type
     * @param <B> the original right type
     * @param <C> the new left type
     * @param <D> the new right type
     * @param leftFn the function to apply to Left values
     * @param rightFn the function to apply to Right values
     * @param either the Either value to map over
     * @return the mapped Either value
     */
    public static <A, B, C, D> hydra.core.overlay.java.util.Either<C, D> apply(
            Function<A, C> leftFn,
            Function<B, D> rightFn,
            hydra.core.overlay.java.util.Either<A, B> either) {
        if (either.isLeft()) {
            return new hydra.core.overlay.java.util.Either.Left<>(leftFn.apply(((hydra.core.overlay.java.util.Either.Left<A, B>) either).value));
        } else {
            return new hydra.core.overlay.java.util.Either.Right<>(rightFn.apply(((hydra.core.overlay.java.util.Either.Right<A, B>) either).value));
        }
    }
}
