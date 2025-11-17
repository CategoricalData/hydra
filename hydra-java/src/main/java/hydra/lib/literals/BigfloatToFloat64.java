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

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;

/**
 * Primitive function which converts a bigfloat (arbitrary-precision decimal) to a float64 (64-bit floating-point).
 * This conversion may result in loss of precision.
 */
public class BigfloatToFloat64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigfloatToFloat64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigfloatToFloat64");
    }

    /**
     * Returns the type scheme for this function: bigfloat -&gt; float64.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigfloat(), Types.float64()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigfloat terms to float64 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigfloat(args.get(0)), s -> Terms.float64(apply(s)));
    }

    /**
     * Converts a BigDecimal value to a Double (64-bit).
     * @param value the BigDecimal value to convert
     * @return the Double representation of the value
     */
    public static Double apply(BigDecimal value) {
        return value.doubleValue();
    }
}
