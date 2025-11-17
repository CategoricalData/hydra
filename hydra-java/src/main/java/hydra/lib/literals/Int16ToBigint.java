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
 * Primitive function which converts an int16 (16-bit signed integer) to a bigint (arbitrary-precision integer).
 * This conversion is lossless.
 */
public class Int16ToBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.int16ToBigint"
     */
    public Name name() {
        return new Name("hydra.lib.literals.int16ToBigint");
    }

    /**
     * Returns the type scheme for this function: int16 -&gt; bigint.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.int16(), Types.bigint()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int16 terms to bigint terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int16(args.get(0)), s -> Terms.bigint(apply(s)));
    }

    /**
     * Converts a Short (16-bit signed integer) value to a BigInteger.
     * @param value the Short value to convert
     * @return the BigInteger representation of the value
     */
    public static BigInteger apply(Short value) {
        return BigInteger.valueOf(value);
    }
}
