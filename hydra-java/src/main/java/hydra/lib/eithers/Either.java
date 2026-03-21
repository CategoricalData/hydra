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
import hydra.errors.Error_;

/**
 * Eliminate an Either value by applying one of two functions.
 */
public class Either extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.either");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c",
            function(
                function(var("a"), var("c")),
                function(var("b"), var("c")),
                either(var("a"), var("b")),
                var("c")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, hydra.util.Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term forLeft = args.get(0);
            Term forRight = args.get(1);
            return hydra.lib.eithers.Map.apply(e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        return Terms.apply(forLeft, left.value);
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        return Terms.apply(forRight, right.value);
                    }
                }), hydra.extract.core.Core.eitherTerm(cx, t -> hydra.util.Either.right(t), t -> hydra.util.Either.right(t), graph, args.get(2)));
        };
    }

    /**
     * Eliminate an Either value by applying one of two functions depending on whether it is a Left or Right.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param <C> the result type
     * @param leftFn the function to apply to Left values
     * @param rightFn the function to apply to Right values
     * @param either the Either value to eliminate
     * @return the result of applying the appropriate function
     */
    public static <A, B, C> C apply(
            Function<A, C> leftFn,
            Function<B, C> rightFn,
            hydra.util.Either<A, B> either) {
        if (either.isLeft()) {
            return leftFn.apply(((hydra.util.Either.Left<A, B>) either).value);
        } else {
            return rightFn.apply(((hydra.util.Either.Right<A, B>) either).value);
        }
    }
}
