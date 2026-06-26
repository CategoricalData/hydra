package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a float64 (IEEE 754 double) to a decimal (arbitrary-precision exact decimal).
 * Uses BigDecimal.valueOf to preserve the canonical decimal representation of the double.
 */
public class Float64ToDecimal extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Literals.float64ToDecimal().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float64(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(d -> Terms.decimal(apply(d)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    public static BigDecimal apply(Double value) {
        return BigDecimal.valueOf(value);
    }
}
