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
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Rounds a float64 to n significant digits.
 */
public class RoundFloat64 extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Math_.roundFloat64().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.int32(graph, args.get(0)), arg0 -> hydra.core.overlay.java.lib.eithers.Map.apply(arg1 -> Terms.float64(apply(arg0, arg1)), hydra.core.extract.Core.float64(graph, args.get(1))));
    }

    public static Function<Double, Double> apply(int n) {
        return (x) -> apply(n, x);
    }

    public static double apply(int n, double x) {
        if (Double.isNaN(x) || Double.isInfinite(x)) {
            return x;
        }
        if (x == 0) {
            return 0.0;
        }
        double factor = Math.pow(10, n - 1 - Math.floor(Math.log10(Math.abs(x))));
        return Math.round(x * factor) / factor;
    }
}
