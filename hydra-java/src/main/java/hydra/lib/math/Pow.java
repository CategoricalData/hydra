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
 * Raises a number to a power.
 */
public class Pow extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.pow");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.float64(args.get(0)), Expect.float64(args.get(1)),
            (arg0, arg1) -> Terms.float64(apply(arg0, arg1)));
    }

    /**
     * Computes base^exponent.
     * @param x the base
     * @return the result
     */
        public static Function<Double, Double> apply(Double x) {
        return (y) -> apply(x, y);
    }

    /**
     * Computes base^exponent.
     * @param x the base
     * @param y the exponent
     * @return the result
     */
        public static Double apply(Double x, Double y) {
        return Math.pow(x, y);
    }
}
