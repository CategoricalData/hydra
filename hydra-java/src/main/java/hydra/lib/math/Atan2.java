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
 * Computes the arctangent of y/x.
 */
public class Atan2 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.atan2");
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
     * Computes the arctangent of y/x.
     * @param y the y
     * @return the arctangent
     */
        public static Function<Double, Double> apply(Double y) {
        return (x) -> apply(y, x);
    }

    /**
     * Computes the arctangent of y/x.
     * @param y the y
     * @param x the x
     * @return the arctangent
     */
        public static Double apply(Double y, Double x) {
        return Math.atan2(y, x);
    }
}
