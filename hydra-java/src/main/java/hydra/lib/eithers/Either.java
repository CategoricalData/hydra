package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term forLeft = args.get(0);
            Term forRight = args.get(1);
            return Flows.map(Expect.<Graph, Term, Term>either(args.get(2)),
                e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        return Terms.apply(forLeft, left.value);
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        return Terms.apply(forRight, right.value);
                    }
                }));
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
