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
 * Primitive function which converts a decimal (arbitrary-precision exact decimal) to a float32 (IEEE 754 single).
 * This conversion may lose precision for values that cannot be represented exactly as a float.
 */
public class DecimalToFloat32 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.decimalToFloat32");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.decimal(), Types.float32()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(d -> Terms.float32(apply(d)), hydra.extract.Core.decimal(graph, args.get(0)));
    }

    public static Float apply(BigDecimal value) {
        return value.floatValue();
    }
}
