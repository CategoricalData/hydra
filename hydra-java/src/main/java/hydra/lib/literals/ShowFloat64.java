package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Primitive function which converts a float64 (64-bit floating-point) to its string representation.
 */
public class ShowFloat64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showFloat64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showFloat64");
    }

    /**
     * Returns the type scheme for this function: float64 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(float64(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts float64 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<Double, Term>) d -> Terms.string(apply(d)), hydra.extract.core.Core.float64(cx, graph, args.get(0)));
    }

    /**
     * Converts a Double (64-bit) value to its string representation.
     * @param value the Double value to convert
     * @return the string representation of the value
     */
    public static String apply(Double value) {
        return ShowFloat.showFloat(value);
    }
}
