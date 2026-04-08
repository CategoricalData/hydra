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
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Computes the inverse hyperbolic sine.
 */
public class Asinh extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.asinh");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((arg0) -> Terms.float64(apply(arg0)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    /**
     * Computes the inverse hyperbolic sine.
     * @param x the value
     * @return the inverse hyperbolic sine
     */
    public static Double apply(Double x) {
        // Special-case infinities to match Haskell: asinh(±Inf) = ±Inf.
        // The naive formula gives log(-Inf + Inf) = NaN for -Inf input.
        if (Double.isInfinite(x)) {
            return x;
        }
        return Math.log(x + Math.sqrt(x * x + 1.0));
    }
}
