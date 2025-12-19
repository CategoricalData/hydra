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
 * Map over both sides of an Either value.
 */
public class Bimap extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.bimap");

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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term forLeft = args.get(0);
            Term forRight = args.get(1);
            return Flows.map(Expect.<Graph, Term, Term>either(args.get(2)),
                e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        return Terms.left(Terms.apply(forLeft, left.value));
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        return Terms.right(Terms.apply(forRight, right.value));
                    }
                }));
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
    public static <A, B, C, D> hydra.util.Either<C, D> apply(
            Function<A, C> leftFn,
            Function<B, D> rightFn,
            hydra.util.Either<A, B> either) {
        if (either.isLeft()) {
            return new hydra.util.Either.Left<>(leftFn.apply(((hydra.util.Either.Left<A, B>) either).value));
        } else {
            return new hydra.util.Either.Right<>(rightFn.apply(((hydra.util.Either.Right<A, B>) either).value));
        }
    }
}
