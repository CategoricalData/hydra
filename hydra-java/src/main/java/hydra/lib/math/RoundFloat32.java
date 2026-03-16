package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float32;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Rounds a float32 to n significant digits.
 */
public class RoundFloat32 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.roundFloat32");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), float32(), float32()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.int32(cx, graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.float32(apply(arg0, arg1)), hydra.extract.core.Core.float32(cx, graph, args.get(1))));
    }

    public static Function<Float, Float> apply(int n) {
        return (x) -> apply(n, x);
    }

    public static float apply(int n, float x) {
        if (x == 0) {
            return 0.0f;
        }
        double factor = Math.pow(10, n - 1 - Math.floor(Math.log10(Math.abs(x))));
        return (float) (Math.round(x * factor) / factor);
    }
}
