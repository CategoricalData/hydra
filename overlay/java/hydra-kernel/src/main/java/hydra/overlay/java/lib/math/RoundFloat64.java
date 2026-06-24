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
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


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
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), arg0 -> hydra.overlay.java.lib.eithers.Map.apply(arg1 -> Terms.float64(apply(arg0, arg1)), hydra.extract.Core.float64(graph, args.get(1))));
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
