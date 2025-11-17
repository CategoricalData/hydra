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
 * Primitive function which converts a bigint (arbitrary-precision integer) to a uint16 (16-bit unsigned integer).
 * This conversion may result in loss of precision if the value is outside the uint16 range.
 */
public class BigintToUint16 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigintToUint16"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigintToUint16");
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; uint16.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.uint16()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to uint16 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigint(args.get(0)), s -> Terms.uint16(apply(s)));
    }

    /**
     * Converts a BigInteger value to a Character (16-bit unsigned integer).
     * @param value the BigInteger value to convert
     * @return the Character representation of the value
     */
    public static Character apply(BigInteger value) {
        return Character.valueOf((char) value.intValue());
    }
}
