package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Computes the arctangent of y/x.
 */
public class Atan2 extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.atan2");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.float64(cx, graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.float64(apply(arg0, arg1)), hydra.extract.core.Core.float64(cx, graph, args.get(1))));
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
