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
import static hydra.dsl.Types.int8;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which converts an int8 (8-bit signed integer) to its string representation.
 */
public class ShowInt8 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showInt8"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showInt8");
    }

    /**
     * Returns the type scheme for this function: int8 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int8(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int8 terms to string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int8(args.get(0)),
            (Function<Byte, Term>) b -> Terms.string(apply(b)));
    }

    /**
     * Converts a Byte (8-bit signed integer) value to its string representation.
     * @param value the Byte value to convert
     * @return the string representation of the value
     */
    public static String apply(Byte value) {
        return Byte.toString(value);
    }
}
