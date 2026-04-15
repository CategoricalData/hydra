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
 * Primitive function which converts a float32 (IEEE 754 single) to a decimal (arbitrary-precision exact decimal).
 * Uses Float.toString so the decimal matches the float's canonical textual form.
 */
public class Float32ToDecimal extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.float32ToDecimal");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float32(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(f -> Terms.decimal(apply(f)), hydra.extract.Core.float32(graph, args.get(0)));
    }

    public static BigDecimal apply(Float value) {
        return new BigDecimal(Float.toString(value));
    }
}
