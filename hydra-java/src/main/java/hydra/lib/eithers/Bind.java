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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term funTerm = args.get(1);
            return Flows.map(Expect.<Graph, Term, Term>either(args.get(0)),
                e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        // Return Left unchanged
                        return Terms.left(left.value);
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        // Apply function to Right value
                        return Terms.apply(funTerm, right.value);
                    }
                }));
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
    public static <A, B, C> hydra.util.Either<A, C> apply(
            hydra.util.Either<A, B> either,
            Function<B, hydra.util.Either<A, C>> fn) {
        if (either.isLeft()) {
            @SuppressWarnings("unchecked")
            hydra.util.Either<A, C> result = (hydra.util.Either<A, C>) either;
            return result;
        } else {
            return fn.apply(((hydra.util.Either.Right<A, B>) either).value);
        }
    }
}
