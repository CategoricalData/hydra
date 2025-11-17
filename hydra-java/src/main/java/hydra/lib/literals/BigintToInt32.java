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
 * Primitive function which converts a bigint (arbitrary-precision integer) to an int32 (32-bit signed integer).
 * This conversion may result in loss of precision if the value is outside the int32 range.
 */
public class BigintToInt32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigintToInt32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigintToInt32");
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; int32.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to int32 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigint(args.get(0)), s -> Terms.int32(apply(s)));
    }

    /**
     * Converts a BigInteger value to an Integer (32-bit signed integer).
     * @param value the BigInteger value to convert
     * @return the Integer representation of the value
     */
    public static Integer apply(BigInteger value) {
        return value.intValue();
    }
}
