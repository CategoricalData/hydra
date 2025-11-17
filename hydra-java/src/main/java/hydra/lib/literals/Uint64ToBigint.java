package hydra.lib.literals;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;

/**
 * Primitive function which converts a uint64 (64-bit unsigned integer) to a bigint (arbitrary-precision integer).
 * This conversion is essentially a type cast as both are represented by BigInteger.
 */
public class Uint64ToBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.uint64ToBigint"
     */
    public Name name() {
        return new Name("hydra.lib.literals.uint64ToBigint");
    }

    /**
     * Returns the type scheme for this function: uint64 -&gt; bigint.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.uint64(), Types.bigint()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts uint64 terms to bigint terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.uint64(args.get(0)), s -> Terms.bigint(apply(s)));
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
