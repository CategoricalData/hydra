package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a decimal (arbitrary-precision exact decimal) to a bigint
 * (arbitrary-precision integer). This conversion truncates any fractional part of the decimal value.
 */
public class DecimalToBigint extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.decimalToBigint");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.decimal(), Types.bigint()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(d -> Terms.bigint(apply(d)), hydra.extract.Core.decimal(graph, args.get(0)));
    }

    public static BigInteger apply(BigDecimal value) {
        return value.setScale(0, RoundingMode.HALF_EVEN).toBigInteger();
    }
}
