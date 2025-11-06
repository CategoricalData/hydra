package hydra.lib.math;

import hydra.dsl.Flows;
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

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Computes the hyperbolic tangent.
 */
public class Tanh extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.tanh");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.float64(args.get(0)),
            (arg0) -> Terms.float64(apply(arg0)));
    }

    /**
     * Computes the hyperbolic tangent.
     * @param x the value
     * @return the hyperbolic tangent
     */
        public static Double apply(Double x) {
        return Math.tanh(x);
    }
}
