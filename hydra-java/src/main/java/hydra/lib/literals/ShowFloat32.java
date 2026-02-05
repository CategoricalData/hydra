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

import static hydra.dsl.Types.float32;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which converts a float32 (32-bit floating-point) to its string representation.
 */
public class ShowFloat32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showFloat32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showFloat32");
    }

    /**
     * Returns the type scheme for this function: float32 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(float32(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts float32 terms to string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.float32(args.get(0)),
            (Function<Float, Term>) f -> Terms.string(apply(f)));
    }

    /**
     * Converts a Float (32-bit) value to its string representation.
     * @param value the Float value to convert
     * @return the string representation of the value
     */
    public static String apply(Float value) {
        return ShowFloat.showFloat32(value);
    }
}
