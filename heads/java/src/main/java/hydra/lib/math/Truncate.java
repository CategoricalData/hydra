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
 * Truncates toward zero, returned as a float.
 * <p>
 * DIVERGENCE FROM HASKELL: returns a Float64 rather than an Integer so that
 * NaN and ±Inf propagate naturally per IEEE 754 (see Ceiling for details).
 */
public class Truncate extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.truncate");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((arg0) -> Terms.float64(apply(arg0)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    /**
     * Truncates toward zero, returned as a float.
     * NaN and ±Inf inputs are returned unchanged.
     */
    public static Double apply(Double x) {
        if (Double.isNaN(x) || Double.isInfinite(x)) {
            return x;
        }
        return x >= 0 ? Math.floor(x) : Math.ceil(x);
    }
}
