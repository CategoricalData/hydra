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
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Rounds a float64 to n significant digits.
 */
public class RoundFloat64 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.roundFloat64");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), float64(), float64()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(cx, graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.float64(apply(arg0, arg1)), hydra.extract.Core.float64(cx, graph, args.get(1))));
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
