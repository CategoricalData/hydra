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
 * Extract the Left value from an Either, or return a default value.
 */
public class FromLeft extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.fromLeft");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(var("a"), either(var("a"), var("b")), var("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term defaultValue = args.get(0);
            return Flows.map(Expect.<Graph, Term, Term>either(args.get(1)),
                e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        return left.value;
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        return defaultValue;
                    }
                }));
        };
    }

    /**
     * Extract the Left value from an Either, or return a default value if it is a Right.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param defaultValue the default value to return if the Either is a Right
     * @param either the Either value to extract from
     * @return the Left value or the default
     */
    public static <A, B> A apply(A defaultValue, hydra.util.Either<A, B> either) {
        if (either.isLeft()) {
            return ((hydra.util.Either.Left<A, B>) either).value;
        } else {
            return defaultValue;
        }
    }
}
