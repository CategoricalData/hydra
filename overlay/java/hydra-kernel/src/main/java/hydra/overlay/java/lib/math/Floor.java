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
 * Rounds down to the nearest integer, returned as a float.
 * <p>
 * DIVERGENCE FROM HASKELL: returns a Float64 rather than an Integer so that
 * NaN and ±Inf propagate naturally per IEEE 754 (see Ceiling for details).
 */
public class Floor extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Math_.floor().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((arg0) -> Terms.float64(apply(arg0)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    /**
     * Rounds down to the nearest integer, returned as a float.
     * NaN and ±Inf inputs are returned unchanged.
     */
    public static Double apply(Double x) {
        if (Double.isNaN(x) || Double.isInfinite(x)) {
            return x;
        }
        return Math.floor(x);
    }
}
