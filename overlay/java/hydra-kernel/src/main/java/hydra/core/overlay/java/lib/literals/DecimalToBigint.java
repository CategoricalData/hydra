package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Primitive function which converts a decimal (arbitrary-precision exact decimal) to a bigint
 * (arbitrary-precision integer). This conversion truncates any fractional part of the decimal value.
 */
public class DecimalToBigint extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Literals.decimalToBigint().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.decimal(), Types.bigint()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(d -> Terms.bigint(apply(d)), hydra.core.extract.Model.decimal(graph, args.get(0)));
    }

    public static BigInteger apply(BigDecimal value) {
        return value.setScale(0, RoundingMode.HALF_EVEN).toBigInteger();
    }
}
