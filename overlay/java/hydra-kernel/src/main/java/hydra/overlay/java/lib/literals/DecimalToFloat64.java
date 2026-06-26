package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a decimal (arbitrary-precision exact decimal) to a float64 (IEEE 754 double).
 * This conversion may lose precision for values that cannot be represented exactly as a double.
 */
public class DecimalToFloat64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.decimalToFloat64"
     */
    public Name name() {
        return hydra.lib.Literals.decimalToFloat64().name;
    }

    /**
     * Returns the type scheme for this function: decimal -&gt; float64.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.decimal(), Types.float64()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts decimal terms to float64 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(d -> Terms.float64(apply(d)), hydra.extract.Core.decimal(graph, args.get(0)));
    }

    /**
     * Converts a BigDecimal value to a Double (IEEE 754 double).
     * @param value the BigDecimal value to convert
     * @return the Double representation of the value
     */
    public static Double apply(BigDecimal value) {
        return value.doubleValue();
    }
}
