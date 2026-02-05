package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint64;


/**
 * Primitive function which converts a uint64 (64-bit unsigned integer) to its string representation.
 */
public class ShowUint64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showUint64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showUint64");
    }

    /**
     * Returns the type scheme for this function: uint64 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(uint64(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts uint64 terms to string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.uint64(args.get(0)),
            (Function<BigInteger, Term>) i -> Terms.string(apply(i)));
    }

    /**
     * Converts a BigInteger (used to represent 64-bit unsigned integer) to its string representation.
     * @param value the BigInteger value to convert
     * @return the string representation of the value
     */
    public static String apply(BigInteger value) {
        return value.toString();
    }
}
