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
 * Extract the Left value from an Either, or return a default value.
 */
public class FromLeft extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.fromLeft");

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
            function(var("a"), either(var("a"), var("b")), var("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term defaultValue = args.get(0);
            return hydra.overlay.java.lib.eithers.Map.apply(e -> e.accept(new hydra.overlay.java.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.overlay.java.util.Either.Left<Term, Term> left) {
                        return left.value;
                    }

                    @Override
                    public Term visit(hydra.overlay.java.util.Either.Right<Term, Term> right) {
                        return defaultValue;
                    }
                }), hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
        };
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Either)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <A, B> A apply(A defaultValue, hydra.overlay.java.util.Either<A, B> either) {
        if (either.isLeft()) {
            return ((hydra.overlay.java.util.Either.Left<A, B>) either).value;
        } else {
            return defaultValue;
        }
    }

    /**
     * Lazily extract the Left value from an Either, or return a default value if it is a Right.
     * The default is only evaluated if the Either is a Right.
     */
    public static <A, B> A applyLazy(Supplier<A> defaultValue, hydra.overlay.java.util.Either<A, B> either) {
        if (either.isLeft()) {
            return ((hydra.overlay.java.util.Either.Left<A, B>) either).value;
        } else {
            return defaultValue.get();
        }
    }
}
