package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.float64;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Computes the logarithm with a specified base.
 */
public class LogBase extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.logBase().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.float64(graph, args.get(0)), arg0 -> hydra.overlay.java.lib.eithers.Map.apply(arg1 -> Terms.float64(apply(arg0, arg1)), hydra.extract.Core.float64(graph, args.get(1))));
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
