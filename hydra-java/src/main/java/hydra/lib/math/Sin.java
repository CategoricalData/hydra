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
 * Computes the sine of an angle.
 */
public class Sin extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.sin");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.float64(args.get(0)),
            (arg0) -> Terms.float64(apply(arg0)));
    }

    /**
     * Computes the sine.
     * @param x the angle
     * @return the sine value
     */
    public static Double apply(Double x) {
        return Math.sin(x);
    }
}
