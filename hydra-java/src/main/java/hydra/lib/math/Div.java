package hydra.lib.math;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Flows.bind2;
import static hydra.dsl.Flows.fail;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;

/**
 * Performs division on two numbers.
 */
public class Div extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.div");
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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind2(Expect.int32(args.get(0)), Expect.int32(args.get(1)),
            (BiFunction<Integer, Integer, Flow<Graph, Term>>) (arg0, arg1) -> {
                if (arg1.equals(0)) {
                    return fail("division by zero");
                } else {
                    return pure(Terms.int32(apply(arg0, arg1)));
                }
            });
    }

    /**
     * Divides the first number by the second.
     * @param dividend the dividend
     * @return the quotient
     */
    public static Function<Integer, Integer> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Divides the first number by the second.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return the quotient
     */
    public static Integer apply(Integer dividend, Integer divisor) {
        return (dividend / divisor);
    }
}
