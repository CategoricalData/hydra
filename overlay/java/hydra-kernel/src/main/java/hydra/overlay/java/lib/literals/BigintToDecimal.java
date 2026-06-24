package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to a decimal
 * (arbitrary-precision exact decimal).
 */
public class BigintToDecimal extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Literals.bigintToDecimal().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(i -> Terms.decimal(apply(i)), hydra.extract.Core.bigint(graph, args.get(0)));
    }

    public static BigDecimal apply(BigInteger value) {
        return new BigDecimal(value);
    }
}
