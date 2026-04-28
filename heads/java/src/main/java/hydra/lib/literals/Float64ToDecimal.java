package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a float64 (IEEE 754 double) to a decimal (arbitrary-precision exact decimal).
 * Uses BigDecimal.valueOf to preserve the canonical decimal representation of the double.
 */
public class Float64ToDecimal extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.float64ToDecimal");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float64(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(d -> Terms.decimal(apply(d)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    public static BigDecimal apply(Double value) {
        return BigDecimal.valueOf(value);
    }
}
