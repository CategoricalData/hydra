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

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int64;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which converts an int64 (64-bit signed integer) to its string representation.
 */
public class ShowInt64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showInt64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showInt64");
    }

    /**
     * Returns the type scheme for this function: int64 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int64(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int64 terms to string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int64(args.get(0)),
            (Function<Long, Term>) l -> Terms.string(apply(l)));
    }

    /**
     * Converts a Long (64-bit signed integer) value to its string representation.
     * @param value the Long value to convert
     * @return the string representation of the value
     */
    public static String apply(Long value) {
        return Long.toString(value);
    }
}
