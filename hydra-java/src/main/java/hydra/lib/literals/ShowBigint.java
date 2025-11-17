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

import static hydra.dsl.Types.bigint;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to its string representation.
 */
public class ShowBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showBigint"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showBigint");
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(bigint(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigint(args.get(0)),
            (Function<BigInteger, Term>) i -> Terms.string(apply(i)));
    }

    /**
     * Converts a BigInteger value to its string representation.
     * @param value the BigInteger value to convert
     * @return the string representation of the value
     */
    public static String apply(BigInteger value) {
        return value.toString();
    }
}
