package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.util.Either;


/**
 * Computes the remainder of integer division.
 */
public class Rem extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.rem");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), int32()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)),
            arg0 -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(1)),
            arg1 -> {
                if (arg1.equals(0)) {
                    return Either.left(new Error_.Other(new OtherError("division by zero")));
                } else {
                    return Either.right(Terms.int32(apply(arg0, arg1)));
                }
            }));
    }

    /**
     * Computes the remainder.
     * @param dividend the dividend
     * @return the remainder
     */
    public static Function<Integer, Integer> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Computes the remainder.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return the remainder
     */
    public static Integer apply(Integer dividend, Integer divisor) {
        // % in Java is a mathematical remainder, not modulus
        return (dividend % divisor);
    }
}
