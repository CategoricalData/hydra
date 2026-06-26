package hydra.overlay.java.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Extract the Right value from an Either, or return a default value.
 */
public class FromRight extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.fromRight");

    public Name name() {
        return NAME;
    }

    @Override
    protected List<Integer> lazyParams() {
        return List.of(0);
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(var("b"), either(var("a"), var("b")), var("b")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term defaultValue = args.get(0);
            return hydra.overlay.java.lib.eithers.Map.apply(e -> e.accept(new hydra.overlay.java.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.overlay.java.util.Either.Left<Term, Term> left) {
                        return defaultValue;
                    }

                    @Override
                    public Term visit(hydra.overlay.java.util.Either.Right<Term, Term> right) {
                        return right.value;
                    }
                }), hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
        };
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Either)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <A, B> B apply(B defaultValue, hydra.overlay.java.util.Either<A, B> either) {
        if (either.isRight()) {
            return ((hydra.overlay.java.util.Either.Right<A, B>) either).value;
        } else {
            return defaultValue;
        }
    }

    /**
     * Lazily extract the Right value from an Either, or return a default value if it is a Left.
     * The default is only evaluated if the Either is a Left.
     */
    public static <A, B> B applyLazy(Supplier<B> defaultValue, hydra.overlay.java.util.Either<A, B> either) {
        if (either.isRight()) {
            return ((hydra.overlay.java.util.Either.Right<A, B>) either).value;
        } else {
            return defaultValue.get();
        }
    }
}
