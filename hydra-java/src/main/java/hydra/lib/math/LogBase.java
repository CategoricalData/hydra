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
import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Computes the logarithm with a specified base.
 */
public class LogBase extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.logBase");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64(), float64()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.float64(args.get(0)), Expect.float64(args.get(1)),
            (arg0, arg1) -> Terms.float64(apply(arg0, arg1)));
    }

    /**
     * Computes the logarithm with the given base.
     * @param base the base
     * @return the logarithm
     */
    public static Function<Double, Double> apply(Double base) {
        return (x) -> apply(base, x);
    }

    /**
     * Computes the logarithm with the given base.
     * @param base the base
     * @param x the value
     * @return the logarithm
     */
    public static Double apply(Double base, Double x) {
        return Math.log(x) / Math.log(base);
    }
}
