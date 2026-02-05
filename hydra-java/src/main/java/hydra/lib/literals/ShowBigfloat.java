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

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.bigfloat;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which converts a bigfloat (arbitrary-precision decimal) to its string representation.
 */
public class ShowBigfloat extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showBigfloat"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showBigfloat");
    }

    /**
     * Returns the type scheme for this function: bigfloat -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(bigfloat(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigfloat terms to string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigfloat(args.get(0)),
            (Function<BigDecimal, Term>) d -> Terms.string(apply(d)));
    }

    /**
     * Converts a BigDecimal value to its string representation.
     * @param value the BigDecimal value to convert
     * @return the string representation of the value
     */
    public static String apply(BigDecimal value) {
        return ShowFloat.showFloat(value.doubleValue());
    }
}
