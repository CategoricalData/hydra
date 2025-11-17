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
import java.util.function.Function;

import static hydra.dsl.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;

/**
 * Computes the modulo of two integers.
 */
public class Mod extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.mod");
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
        return args -> map2(Expect.int32(args.get(0)), Expect.int32(args.get(1)),
            (arg0, arg1) -> Terms.int32(apply(arg0, arg1)));
    }

    /**
     * Computes the modulo.
     * @param dividend the dividend
     * @return the modulo
     */
    public static Function<Integer, Integer> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Computes the modulo.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return the modulo
     */
    public static Integer apply(Integer dividend, Integer divisor) {
        return java.lang.Math.floorMod(dividend, divisor);
    }
}
