package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to a uint64 (64-bit unsigned integer).
 * This conversion is essentially a type cast as both are represented by BigInteger.
 */
public class BigintToUint64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigintToUint64"
     */
    public Name name() {
        return hydra.lib.Literals.bigintToUint64().name;
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; uint64.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.uint64()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to uint64 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(s -> Terms.uint64(apply(s)), hydra.extract.Core.bigint(graph, args.get(0)));
    }

    /**
     * Converts a BigInteger value to a BigInteger (identity function for uint64).
     * @param value the BigInteger value to convert
     * @return the same BigInteger value
     */
    public static BigInteger apply(BigInteger value) {
        return value;
    }
}
