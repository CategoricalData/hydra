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
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to a bigfloat (arbitrary-precision decimal).
 * This conversion is lossless.
 */
public class BigintToBigfloat extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigintToBigfloat"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigintToBigfloat");
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; bigfloat.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.bigfloat()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to bigfloat terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.bigfloat(apply(s)), hydra.extract.Core.bigint(cx, graph, args.get(0)));
    }

    /**
     * Converts a BigInteger value to a BigDecimal.
     * @param value the BigInteger value to convert
     * @return the BigDecimal representation of the value
     */
    public static BigDecimal apply(BigInteger value) {
        return new BigDecimal(value);
    }
}
