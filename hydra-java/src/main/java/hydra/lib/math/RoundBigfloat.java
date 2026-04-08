package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.bigfloat;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Rounds a bigfloat to n significant digits.
 */
public class RoundBigfloat extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.roundBigfloat");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), bigfloat(), bigfloat()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.bigfloat(apply(arg0, arg1)), hydra.extract.Core.bigfloat(graph, args.get(1))));
    }

    public static Function<BigDecimal, BigDecimal> apply(int n) {
        return (x) -> apply(n, x);
    }

    public static BigDecimal apply(int n, BigDecimal x) {
        // Convert through double to match Haskell semantics (where bigfloat is Double)
        return BigDecimal.valueOf(RoundFloat64.apply(n, x.doubleValue()));
    }
}
