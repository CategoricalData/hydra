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
 * Rounds up to the nearest integer, returned as a float.
 * <p>
 * DIVERGENCE FROM HASKELL: Haskell's Prelude.ceiling returns an Integer, which
 * cannot represent NaN or Inf; its behavior on those inputs is undefined and
 * produces nonsensical gigantic integers. Hydra returns a Float64 so that NaN
 * and ±Inf propagate naturally per IEEE 754, matching the conventions of C,
 * Java's Math.ceil, Go, Rust, and JavaScript.
 */
public class Ceiling extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.ceiling().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((arg0) -> Terms.float64(apply(arg0)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    /**
     * Rounds up to the nearest integer, returned as a float.
     * NaN and ±Inf inputs are returned unchanged.
     * @param x the value
     * @return the ceiling value
     */
    public static Double apply(Double x) {
        if (Double.isNaN(x) || Double.isInfinite(x)) {
            return x;
        }
        return Math.ceil(x);
    }
}
