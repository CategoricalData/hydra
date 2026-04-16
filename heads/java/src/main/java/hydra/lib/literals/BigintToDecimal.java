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
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to a decimal
 * (arbitrary-precision exact decimal).
 */
public class BigintToDecimal extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.bigintToDecimal");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(i -> Terms.decimal(apply(i)), hydra.extract.Core.bigint(graph, args.get(0)));
    }

    public static BigDecimal apply(BigInteger value) {
        return new BigDecimal(value);
    }
}
