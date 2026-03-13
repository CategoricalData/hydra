package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to an int64 (64-bit signed integer).
 * This conversion may result in loss of precision if the value is outside the int64 range.
 */
public class BigintToInt64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigintToInt64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigintToInt64");
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; int64.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.int64()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to int64 terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.int64(apply(s)), hydra.extract.core.Core.bigint(cx, graph, args.get(0)));
    }

    /**
     * Converts a BigInteger value to a Long (64-bit signed integer).
     * @param value the BigInteger value to convert
     * @return the Long representation of the value
     */
    public static Long apply(BigInteger value) {
        return value.longValue();
    }
}
