package hydra.lib.math;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.scheme;


/**
 * Returns the mathematical constant pi.
 */
public class Pi extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.pi");
    }

    @Override
    public TypeScheme type() {
        return scheme(float64());
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.pure(Terms.float64(apply()));
    }

    /**
     * Returns pi.
     * @return the value of pi
     */
        public static Double apply() {
        return Math.PI;
    }
}
