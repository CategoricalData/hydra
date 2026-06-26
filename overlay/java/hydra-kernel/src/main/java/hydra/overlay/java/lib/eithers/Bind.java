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
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Bind (flatMap) for Either: sequence an Either with a function that returns Either.
 */
public class Bind extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.bind");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c",
            function(
                either(var("a"), var("b")),
                function(var("b"), either(var("a"), var("c"))),
                either(var("a"), var("c"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term funTerm = args.get(1);
            return hydra.overlay.java.lib.eithers.Map.apply(e -> e.accept(new hydra.overlay.java.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.overlay.java.util.Either.Left<Term, Term> left) {
                        // Return Left unchanged
                        return Terms.left(left.value);
                    }

                    @Override
                    public Term visit(hydra.overlay.java.util.Either.Right<Term, Term> right) {
                        // Apply function to Right value
                        return Terms.apply(funTerm, right.value);
                    }
                }), hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(0)));
        };
    }

    /**
     * Bind (flatMap) for Either: if Right, apply the function and return its result;
     * if Left, return the Left unchanged.
     *
     * @param <A> the error type (Left)
     * @param <B> the input success type (Right)
     * @param <C> the output success type
     * @param either the Either value
     * @param fn the function to apply to Right values
     * @return the result of binding
     */
    public static <A, B, C> hydra.overlay.java.util.Either<A, C> apply(
            hydra.overlay.java.util.Either<A, B> either,
            Function<B, hydra.overlay.java.util.Either<A, C>> fn) {
        if (either.isLeft()) {
            @SuppressWarnings("unchecked")
            hydra.overlay.java.util.Either<A, C> result = (hydra.overlay.java.util.Either<A, C>) either;
            return result;
        } else {
            return fn.apply(((hydra.overlay.java.util.Either.Right<A, B>) either).value);
        }
    }
}
