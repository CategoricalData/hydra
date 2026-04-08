package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Extract the Right value from an Either, or return a default value.
 */
public class FromRight extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.fromRight");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(var("b"), either(var("a"), var("b")), var("b")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term defaultValue = args.get(0);
            return hydra.lib.eithers.Map.apply(e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        return defaultValue;
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        return right.value;
                    }
                }), hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
        };
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Either)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <A, B> B apply(B defaultValue, hydra.util.Either<A, B> either) {
        if (either.isRight()) {
            return ((hydra.util.Either.Right<A, B>) either).value;
        } else {
            return defaultValue;
        }
    }

    /**
     * Lazily extract the Right value from an Either, or return a default value if it is a Left.
     * The default is only evaluated if the Either is a Left.
     */
    public static <A, B> B applyLazy(Supplier<B> defaultValue, hydra.util.Either<A, B> either) {
        if (either.isRight()) {
            return ((hydra.util.Either.Right<A, B>) either).value;
        } else {
            return defaultValue.get();
        }
    }
}
