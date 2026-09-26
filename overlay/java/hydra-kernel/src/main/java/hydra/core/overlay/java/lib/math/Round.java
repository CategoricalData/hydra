package hydra.core.overlay.java.lib.math;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.float64;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Rounds to the nearest integer (banker's rounding), returned as a float.
 * <p>
 * DIVERGENCE FROM HASKELL: returns a Float64 rather than an Integer so that
 * NaN and ±Inf propagate naturally per IEEE 754 (see Ceiling for details).
 */
public class Round extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Math_.round().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((arg0) -> Terms.float64(apply(arg0)), hydra.core.extract.Model.float64(graph, args.get(0)));
    }

    /**
     * Rounds to the nearest integer using banker's rounding (half-to-even),
     * matching Haskell. NaN and ±Inf inputs are returned unchanged.
     */
    public static Double apply(Double x) {
        if (Double.isNaN(x) || Double.isInfinite(x)) {
            return x;
        }
        return java.math.BigDecimal.valueOf(x)
            .setScale(0, java.math.RoundingMode.HALF_EVEN)
            .doubleValue();
    }
}
