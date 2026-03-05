package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
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
                }), hydra.extract.core.Core.eitherTerm(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
        };
    }

    /**
     * Extract the Right value from an Either, or return a default value if it is a Left.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param defaultValue the default value to return if the Either is a Left
     * @param either the Either value to extract from
     * @return the Right value or the default
     */
    public static <A, B> B apply(B defaultValue, hydra.util.Either<A, B> either) {
        if (either.isRight()) {
            return ((hydra.util.Either.Right<A, B>) either).value;
        } else {
            return defaultValue;
        }
    }
}
