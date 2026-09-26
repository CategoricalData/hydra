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
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to a decimal
 * (arbitrary-precision exact decimal).
 */
public class BigintToDecimal extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Literals.bigintToDecimal().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(i -> Terms.decimal(apply(i)), hydra.core.extract.Model.bigint(graph, args.get(0)));
    }

    public static BigDecimal apply(BigInteger value) {
        return new BigDecimal(value);
    }
}
